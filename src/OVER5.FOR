      SUBROUTINE OVER5 
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      COMMON /a8sw/ A8SW(400)                                           M39.1011
      DIMENSION  AKEY(1), TSTAT(1)                                      M33. 205
      EQUIVALENCE  ( AKEY(1), ADELAY(1) ),  ( TSTAT(1), CRIT(1) )       M33. 206
      EQUIVALENCE  (MONCAR(1),    KNT),   (MONCAR(4),    ISW)           M37.2444
      EQUIVALENCE  (MONCAR(5),  IDIST),   (MONCAR(6),  ITEST)           M37.2445
      EQUIVALENCE  (MONCAR(9), KLOAEP)                                  M37.2446
      REAL*8            TEXT1, TEXT2, TEXT5, TEXT6, TEXT7               M42.2858
      REAL*8            TEXT8,        TEXT13, TEXT14, TEXT15, TEXT16    M28.3809
      REAL*8            TEXT17                                          M42.2859
      CHARACTER*80  BUFFER                                              VAX. 891
      DATA  TEXT1    / 6HNAME   /                                       M33. 208
      DATA  TEXT2    / 6HSWT001 /                                       M42.2860
      DATA  TEXT5    / 6HSTATIS /                                       M12. 266
      DATA  TEXT6    / 4HTICS   /                                       M12. 267
      DATA  TEXT7    / 6HSYSTEM /                                       M12. 268
      DATA  TEXT8    / 4HATIC   /                                       M12. 269
      DATA  TEXT13   / 6HMEASUR /                                       M12. 271
      DATA  TEXT14   / 6HING    /                                       M12. 272
      DATA  TEXT15   / 6HCLOSED /                                       M13.1229
      DATA  TEXT16   / 6HSAME   /                                       M13.1230
      DATA  TEXT17   / 6HTARGET /                                       M13.1231
      IF ( IPRSUP  .GE.  1 )                                            M32.1267
     1 WRITE ( LUNIT6, 4567 )                                           M32.1268
 4567 FORMAT ( 22H BEGIN MODULE "OVER5".  )                             M35. 804
      LSTAT(29) = INONL                                                 M42.2861
      LSTAT(30) = ICHAR                                                 M42.2862
      LSTAT(33) = IFDEP                                                 M42.2863
      LSTAT(34) = JST                                                   M42.2864
      LSTAT(40) = IFSEM                                                 M19. 347
      LSTAT(41) = IADD                                                  M32.1270
      CALL MOVER0 ( CREST(1), LEXCT )                                   M42.2865
      CALL MOVER0 ( TIME1(1), LEXCT )                                   M42.2866
      CALL MOVER0 ( SFREQ(1), LEXCT )                                   M42.2867
      CALL MOVER0 ( XK(1), LPAST )                                      M42.2868
      CALL MOVER0 ( XM(1), LPAST )                                      M42.2869
      OMEGA = 0.0                                                       M38.1105
      IWTENT(IFDEP2) = JST1                                             M42.2870
      IF ( IPRSUP .LE. 0 )  GO TO 6693                                  M42.2871
      IF ( IFDEP .EQ. 0 )  GO TO 6693                                   M42.2872
      IF ( IPRSUP  .GE.  9 )                                            M35. 805
     1 WRITE (LUNIT6, 6656)  ( WEIGHT(I), I=1, JST )                    M35. 806
 6656 FORMAT ( 48H WEIGHT ---- ARRAY OF WEIGHTING FUNCTION STORAGE  ,   M35. 807
     1         /,  ( 1X, 10E13.4 )  )                                   M22.2679
      WRITE (LUNIT6, 6648)                                              M22.2680
 6648 FORMAT ( /, 2X,  3HROW,  3X,  5HISKIP,  2X,  6HIWTENT,            M22.2681
     1         12X,  3HETA,  11X,  4HZINF,  10X,  5HTAILP,  10X,        M22.2682
     2          5HTAILQ,  10X,  5HTAILT,  4X,  11HSUM WEIGHTS   )       M22.2683
      DO 6664  I=1, IFDEP                                               M22.2684
      N1 = 2 * I - 1                                                    M22.2685
      N3 = IWTENT(N1)                                                   M22.2686
      N4 = IWTENT(N1+2) - 1                                             M22.2687
      D1 = 0.0                                                          M22.2688
      IP2 = 2 * LFDEP + I                                               M35. 808
      IP3 = 2 * IP2   - I                                               M35. 809
      DO 6655  J=N3, N4                                                 M22.2689
 6655 D1 = D1 + WEIGHT(J)                                               M22.2690
 6664 WRITE (LUNIT6, 6671)  I, ISKIP(I), IWTENT(I), ETA(I),             M22.2691
     1       ZINF(I), CON1(I), CON1(IP2), CON1(IP3), D1                 M35. 810
 6671 FORMAT ( 1X, I4, 2I8, 6E15.6 )                                    M22.2693
      DO 1983 I = 1, IFDEP                                              M35. 811
      N1 = IFDEP + I                                                    M35. 812
      N2 = 2 * LFDEP + N1                                               M35. 813
      N3 = 4 * LFDEP + N1                                               M35. 814
 1983 WRITE (LUNIT6, 6678)   N1, IWTENT(N1), CON1(N1),                  M35. 815
     1                       CON1(N2), CON1(N3)                         M35. 816
 6678 FORMAT ( 1X, I4, 8X, I8, 10X, 3E15.6 )                            M35. 817
 6693 CONTINUE                                                          M42.2873
C                                                READING IN SWITCH DATA M42.2874
      KSWTCH=0                                                          M42.2875
      KCONST=0                                                          M42.2876
      N4=0                                                              M42.2877
      KPASS = 0                                                         M42.2878
      IDUBE1 = 0                                                        M13.1232
      MK = 0                                                            M39.1012
      LL10 = 10                                                         M22.2699
      LSW2 = LSWTCH + LSWTCH                                            M37.2447
      CALL MOVE0 (INDTV(1), LL10)                                       M36. 328
      CALL MOVE0 ( KDEPSW(1), LSW2 )                                    M37.2448
      CALL MOVER0 ( ADELAY(1), LSW2 )                                   M38.1106
  209 IF ( KILL  .GT.  0 )   GO TO 9200                                 M35. 818
      IF ( IPRSUP  .GE.  5 )                                            M13.1236
     1WRITE (LUNIT6, 76)  KSWTCH, KLOAEP, (KDEPSW(IP), IP = 1, KSWTCH)  M17. 362
   76 FORMAT ( /,  39H READ NEXT SWITCH CARD.  KSWTCH  KLOAEP  ,/,      M19. 348
     1 23X,  2I8  ,/,  ( 1X,  20I6 ) )                                  M19. 349
C     READ INPUT CARD USING CIMAGE.                                     M42.2879
 3483 CALL CIMAGE                                                       M42.2880
      IF ( KOLBEG  .GT.  0 )   GO TO 3503                               M22.2702
      DECODE (80, 35, ABUFF(1) )   IT2, BUS1, BUS2, GUS3, GUS4, CK1,    M12. 275
     1                             A, BUS3, BUS4, BUS5, BUS6,           M13.1239
     2                             JDUBE1, J                            M14.1239
   35 FORMAT (I2,2A6,4E10.0,A6,A4,2A6,2X,2I1)                           M14.1240
      GO TO 3506                                                        M22.2703
 3503 NFRFLD = 1                                                        M22.2704
      CALL FREFLD ( VOLTBC(1) )                                         M31. 778
      IT2 = VOLTBC(1)                                                   M31. 779
      NRIGHT = -1                                                       M22.2707
      NFRFLD = 2                                                        M22.2708
      CALL FREONE ( D1 )                                                M29.1404
      NRIGHT = 0                                                        M22.2710
      BUS1 = TEXTA6(1)                                                  M22.2711
      BUS2 = TEXTA6(2)                                                  M22.2712
      NFRFLD = 4                                                        M22.2713
      CALL FREFLD ( VOLTBC(1) )                                         M31. 780
      GUS3 = VOLTBC(1)                                                  M31. 781
      GUS4 = VOLTBC(2)                                                  M31. 782
      CK1 = VOLTBC(3)                                                   M31. 783
      A   = VOLTBC(4)                                                   M31. 784
      NRIGHT = -2                                                       M22.2719
      NFRFLD = 3                                                        M22.2720
      CALL FREONE ( D1 )                                                M29.1405
      NRIGHT = 0                                                        M22.2722
      BUS3 = TEXTA6(1)                                                  M22.2723
      BUS4 = TEXTA6(2)                                                  M22.2724
      BUS5 = TEXTA6(3)                                                  M22.2725
      BUS6 = TEXTA6(4)                                                  M22.2726
      NFRFLD = 2                                                        M22.2727
      CALL FREFLD ( VOLTBC(1) )                                         M31. 785
      JDUBE1 = VOLTBC(1)                                                M31. 786
      J      = VOLTBC(2)                                                M31. 787
 3506 IF ( BUS1  .NE.  BLANK )   GO TO 3510                             M22.2731
      IF ( BUS2 .EQ. BLANK )  GO TO 213                                 M42.2881
 3510 KSWTCH = KSWTCH + 1                                               M35. 819
      KSWT2  = LSWTCH + KSWTCH                                          M37.2450
      IF ( KSWTCH  .LE.  LSWTCH )   GO TO 3517                          M35. 820
      LSTAT(19) = 3510                                                  M22.2733
      IPRINT = 6                                                        M22.2734
      GO TO 9000                                                        M22.2735
 3517 CRIT(LSWTCH+1-KSWTCH) = 0.0                                               
      IF ( BUS1  .NE.  TEXT1 )  GO TO 8212                              M35. 822
      IF ( NOUTPR .EQ. 0 )  WRITE( LUNIT6, 8233 )  BUS2, KSWTCH         M33. 211
 8233 FORMAT (10H MONIKER ", A6, 21H" IS FOR NEXT SWITCH , I6, 2H .)    M33. 212
      CALL NAMEA6 ( BUS2, N24 )                                                 
      NAMESW(KSWTCH) = N24                                                      
      GO TO 3483                                                        M33. 213
 8212 IF ( BUS1  .NE.  BUS2 )   GO TO 3519                              M37.2451
      KILL = 224                                                        M32.1272
      LSTAT(19) = 3517                                                  M32.1273
      GO TO 9200                                                        M32.1274
 3519 CALL NMINCR ( TEXT2, KSWTCH )                                             
      CALL NAMEA6 ( TEXT2, N24 )                                                
      NAMESW(KSWTCH) = N24                                                      
      ADELAY(KSWTCH)=0.                                                 M42.2887
      ATEMP = A                                                         M24. 609
      N1=0                                                              M42.2888
      N2=0                                                              M42.2889
      DO 210 I=1,NTOT                                                   M42.2890
      IF (BUS1.EQ.BUS(I)) N1=I                                          M42.2891
      IF ( BUS2  .EQ.  BUS(I) )   N2 = I                                M18. 334
  210 CONTINUE                                                          M18. 335
      IPRINT = 1                                                        M42.2892
      IF (N1.NE.0) GO TO 211                                            M42.2893
      NTOT=NTOT+1                                                       M42.2894
      LSTAT(19)=  210                                                   M42.2895
      IF (NTOT.GT.LBUS)GO TO 9000                                       M42.2896
      N1=NTOT                                                           M42.2897
      BUS(N1)=BUS1                                                      M42.2898
  211 IF (N2.NE.0) GO TO 212                                            M42.2899
      NTOT=NTOT+1                                                       M42.2900
      LSTAT(19)=211                                                     M42.2901
      IF (NTOT.GT.LBUS)GO TO 9000                                       M42.2902
      N2=NTOT                                                           M42.2903
      BUS(N2)=BUS2                                                      M42.2904
  212 ISOURC(KSWTCH)=0                                                  M42.2905
      IF ( J .LE. 3 )  GO TO 4228                                       M42.2906
      J = 3                                                             M42.2907
      IF ( NPOWER  .LT.  MAXPE )   GO TO 54201                          M20.1053
      IPRINT = 18                                                       M20.1054
      LSTAT(19) = 54201                                                 M20.1055
      GO TO 9000                                                        M20.1056
54201 NPOWER = NPOWER + 1                                               M20.1057
      MPOWER = NPOWER + MAXPE                                           M35. 823
      KOUTVP(NPOWER) = -( NV+1)                                         M42.2908
      KOUTVP(MPOWER) = KSWTCH                                           M35. 824
      IF ( IPRSUP .GE. 2 )                                              M42.2909
     1WRITE(LUNIT6, 4227)  NPOWER, MAXPE, KOUTVP(NPOWER),               M22.2737
     2  KOUTVP(MPOWER)                                                  M35. 825
 4227 FORMAT ( /, 21H POWER OUTPUT REQUEST   , 4I10 )                   M03. 835
 4228 CONTINUE                                                          M42.2910
      IF (J.LT.2) GO TO 224                                             M42.2911
      NV =NV+1                                                          M42.2912
      IBRNCH(NV) = N1                                                   M42.2913
      JBRNCH(NV) = N2                                                   M42.2914
      IF ( NV  .LE.  LSIZ12 )   GO TO 224                               M38.1107
      IPRINT =11                                                        M42.2915
      LSTAT(19) =  224                                                  M42.2916
      GO TO 9000                                                        M42.2917
  224 IF ( J.EQ.2 ) J=0                                                 M42.2918
      IF ( IT2 .GE. 11  .AND.  IT2 .LE. 13 )   GO TO 250                M28.3810
      IF (IT2.EQ.0) GO TO 218                                           M42.2919
      IF (IT2 .NE. 76)  GO TO 4235                                      M23. 991
      IT2 = 9976                                                        M23. 992
      GO TO 218                                                         M23. 993
 4235 KILL = 7                                                          M42.2920
      LSTAT(19) = 224                                                   M42.2921
      LSTAT(16) = IT2                                                   M42.2922
      GO TO 9200                                                        M42.2923
C     *****   INTERPRETING   ****************************************** M28.3811
C     *****     DIODE  OR  TACS-CONTROLLED VALVE  (TYPE-11)  ********** M28.3812
C     *****     TACS-CONTROLLED SPARK GAP         (TYPE-12)  ********** M28.3813
C     *****     TACS-CONTROLLED ORDINARY SWITCH   (TYPE-13)  ********** M28.3814
  250 IF ( BUS4 .NE. TEXT16 )   GO TO 262                               M13.1244
      IF ( IT2 .NE. 11 )  GO TO 262                                     M28.3815
      IF ( IDUBE1 .NE. 0 )  GO TO 257                                   M14.1250
      KILL = 146                                                        M13.1246
      LSTAT(19) = 250                                                   M13.1247
      GO TO 9200                                                        M13.1248
  262 ADUBE1 = GUS3                                                     M14.1251
      ADUBE2 = GUS4                                                     M14.1252
      ADUBE3 = CK1                                                      M14.1253
      IDUBE1 = 1                                                        M13.1255
  257 IDUBE2 = 0                                                        M14.1254
      IDUBE3 = 0                                                        M14.1255
      IF ( BUS5 .EQ. BLANK )  GO TO 264                                 M14.1256
      IF ( IT2 .EQ. 13 )  GO TO 264                                     M28.3816
      IF ( KTAB .EQ. 0 )  GO TO 259                                     M28.3817
      DO 258  I = 1, KTAB                                               M28.3818
      NDX1 = ILNTAB( KLNTAB + I )                                       M38.1108
      IF ( BUS5 .EQ. TEXVEC( NDX1) )  GO TO 261                         M38.1109
  258 CONTINUE                                                          M13.1262
  259 KILL = 147                                                        M13.1263
      LSTAT(19) = 259                                                   M13.1264
      GO TO 9200                                                        M13.1265
  261 IDUBE2 = I                                                        M28.3821
  264 IF ( BUS6 .EQ. BLANK )  GO TO 251                                 M14.1257
      IF ( KTAB .EQ. 0 )  GO TO 265                                     M28.3822
      DO 266  I = 1, KTAB                                               M28.3823
      NDX1 = ILNTAB( KLNTAB + I )                                       M38.1110
      IF ( BUS6 .EQ. TEXVEC( NDX1) )  GO TO 267                         M38.1111
  266 CONTINUE                                                          M14.1262
  265 KILL = 119                                                        M14.1263
      LSTAT(19) = 265                                                   M14.1264
      GO TO 9200                                                        M14.1265
  267 IDUBE3 = I                                                        M28.3826
  251 IF ( NOUTPR .NE. 0 )  GO TO 25199                                 M28.3827
      IF ( IT2 .NE. 11 )  GO TO 25100                                   M28.3828
      IF ( IDUBE2 .NE. 0 )  GO TO 25110                                 M28.3829
      WRITE (KUNIT6, 252)                                               M35. 828
  252 FORMAT (1H+,  20HDIODE,  NO TACS GRID  )                          M28.3831
      GO TO 25150                                                       M28.3832
25110 NDX1 = ILNTAB( KLNTAB + IDUBE2 )                                  M38.1112
      WRITE (KUNIT6, 253)  TEXVEC( NDX1)                                M38.1113
  253 FORMAT ( 1H+, 19HVALVE, TACS GRID= ',  A6,  1H'  )                M28.3835
      GO TO 25150                                                       M28.3836
25100 IF ( IT2 .NE. 12 )  GO TO 25120                                   M28.3837
      IF ( IDUBE2 .NE. 0 )  GO TO 25105                                 M28.3838
      WRITE (KUNIT6, 25101)                                             M35. 830
25101 FORMAT ( 1H+, 19HGAP,  NO TACS SPARK  )                           M28.3840
      GO TO 25150                                                       M28.3841
25105 NDX1 = ILNTAB( KLNTAB + IDUBE2 )                                  M38.1114
      WRITE (KUNIT6, 25106)  TEXVEC( NDX1)                              M38.1115
25106 FORMAT ( 1H+,  19HGAP, TACS SPARK = ',  A6,  1H'  )               M28.3844
25150 IF ( IDUBE3 .EQ. 0 )  GO TO 25199                                 M28.3845
      NDX1 = ILNTAB( KLNTAB + IDUBE3 )                                  M38.1116
      WRITE (KUNIT6, 25151)  TEXVEC( NDX1)                              M38.1117
25151 FORMAT (1H+, 26X, 16H, TACS CLAMP = ',  A6,  1H'  )               M28.3848
      GO TO 25199                                                       M28.3849
25120 IF ( IDUBE3 .NE. 0 )  GO TO 25125                                 M28.3850
      WRITE (KUNIT6, 25121)                                             M35. 833
25121 FORMAT (1H+, 49HSWITCH, TACS CONTROL SIGNAL MISSING  ????????????)M28.3852
      GO TO 25199                                                       M28.3853
25125 NDX1 = ILNTAB( KLNTAB + IDUBE3 )                                  M38.1118
      WRITE (KUNIT6, 25126)  TEXVEC( NDX1)                              M38.1119
25126 FORMAT ( 1H+, 31HSWITCH, TACS CONTROL SIGNAL = ', A6, 1H' )       M28.3856
25199 CONTINUE                                                          M28.3857
      ADELAY(KSWTCH) = A                                                M22.2739
      ENERGY(KSWTCH) = 0.0                                              M22.2740
      KDEPSW(KSWT2) = 8888                                              M37.2452
      IF ( IT2 .EQ. 12 )  KDEPSW(KSWT2) = 8890                          M37.2453
      IF ( IT2 .EQ. 13 )  KDEPSW(KSWT2) = 8891                          M37.2454
      TOPEN(KSWTCH) = FLTINF                                            M13.1278
      TCLOSE(KSWTCH) = FLTINF                                           M13.1279
      IF ( BUS3  .EQ.  TEXT15 )                                         M13.1280
     1 TCLOSE(KSWTCH) = -1.0                                            M13.1281
      KPOS(KSWTCH) = 11                                                 M13.1282
      IF ( J  .GT.  0 )                                                 M13.1283
     1 KPOS(KSWTCH) = -11                                               M13.1284
      KSWTYP(KSWTCH) = 0                                                M32.1276
      NDX1 = LSWTCH + KSWTCH                                            M35. 835
      NDX2 = LSWTCH + NDX1                                              M35. 836
      NDX3 = LSWTCH + NDX2                                              M35. 837
      ARDUBE(NDX1) = ADUBE1                                             M35. 838
      ARDUBE(NDX2) = ADUBE2                                             M35. 839
      ARDUBE(NDX3) = ADUBE3                                             M35. 840
      ARDUBE(KSWTCH) = -FLTINF                                          M35. 841
      IARDUB(KSWTCH) = IDUBE2                                           M35. 842
      IARDUB(NDX1) = IDUBE3                                             M35. 843
      IARDUB(NDX2) = JDUBE1                                             M35. 844
      IF ( A .NE. 7777. ) GO TO 220                                     M39.1013
      CALL CIMAGE                                                       M39.1014
      DECODE (80, 3355, ABUFF(1)) A8SW(MK+1), A8SW(MK+2),               M39.1015
     1                            A8SW(MK+3), A8SW(MK+8)                M39.1016
 3355 FORMAT ( 4E16.6 )                                                 M39.1017
      A8SW(MK+7) = 0.0                                                  M39.1018
      MK = MK + 8                                                       M39.1019
      ADELAY(KSWTCH) = 7 - MK                                           M39.1020
      IF ( MK .LE. 400 ) GO TO 220                                      M39.1021
      KILL = 1                                                          M39.1022
      WRITE ( LUNIT6, 3434 )                                            M39.1023
 3434 FORMAT( 51H ***** OVERFLOW THE TEMPORARY STORAGE 300 CELLS FOR,   M39.1024
     1        46H VECTOR A6SW NEAR STATEMENT 218 AT OVER5 *****  )      M39.1025
      GO TO 9200                                                        M39.1026
  218 IF ( BUS3  .NE.  TEXT13 )  GO TO 70218                            M13.1294
      IF ( BUS4  .NE.  TEXT14 )  GO TO 70218                            M13.1295
      IF ( NOUTPR .EQ. 0 )                                              M20.1072
     1 WRITE (KUNIT6, 10218 )                                           M35. 845
10218 FORMAT ( 45H+PERMANENTLY-CLOSED SWITCH USED FOR METERING.   )     M12. 281
      GUS3 = -1.0                                                       M12. 282
      GUS4 = FLTINF                                                     M12. 283
      GO TO 220                                                         M12. 284
70218 IF ( NOUTPR  .EQ.  0 )                                            M20.1074
     1 WRITE (KUNIT6, 36 )  GUS3, GUS4, CK1, A                          M35. 846
   36 FORMAT ( 8H+SWITCH.  ,  2X, 4E10.2  )                             M42.2924
      IF ( A      .EQ.  0.0   )   GO TO 220                             M24. 611
      IF ( ATEMP  .EQ.  3333. )   GO TO 220                             M24. 612
  222 ADELAY(KSWTCH) = ABSZ(GUS4)                                       M18. 338
  220 KMSWIT(KSWTCH) = N1                                               M32.1277
      NDX2 = KSWTCH + LSWTCH                                            M32.1278
      KMSWIT(NDX2) = N2                                                 M32.1279
      IF ( IT2 .GE. 11  .AND.  IT2 .LE. 13 )  GO TO 209                 M28.3862
      ICHECK=11                                                         M42.2925
      IF ( A      .EQ.  0.0   )   GO TO 216                             M24. 613
      IF ( ATEMP  .EQ.  3333. )   GO TO 216                             M24. 614
      GUS4 = ABSZ(A)                                                    M18. 339
      ICHECK=10                                                         M42.2926
      IF (GUS3.GE.0.) GO TO 216                                         M05. 366
      GUS3=0.                                                           M05. 367
      IF ( IPRSUP .GE. 1 )   WRITE(LUNIT6, 217)                         M17. 378
  217 FORMAT ( 16X, 22HTCLOSE CHANGED TO ZERO  )                        M42.2927
  216 IF (J.GT.0) ICHECK=-ICHECK                                        M42.2928
      KPOS(KSWTCH) = ICHECK                                             M42.2929
      KSWTYP(KSWTCH) = IT2                                              M32.1280
      TCLOSE(KSWTCH) =GUS3                                              M05. 368
      TOPEN(KSWTCH) =GUS4                                               M05. 369
      CRIT(KSWTCH) =CK1                                                 M42.2930
      DECODE (80, 3898, ABUFF)  BUFFER                                  VAX. 892
 3898 FORMAT ( A80 )                                                    VAX. 893
      N98 = INDEX ( BUFFER, '   STATISTICS' )                           VAX. 894
      N99 = INDEX ( BUFFER, '   ST'         )                           VAX. 895
      IF ( N98 .EQ. 52  .OR.                                            VAX. 896
     1     N99 .EQ. 0 ) GO TO 3920                                      VAX. 897
      IF ( N99 .LT. 30  .OR.                                            VAX. 898
     1     N99 .GT. 60 ) GO TO 3920                                     VAX. 899
      WRITE (LUNIT6, 3906)  BUFFER                                      VAX. 900
 3906 FORMAT ( ' Temporary error stop in  OVER5.   The user"s',         VAX. 901
     1         ' data is for a Monte Carlo simulation,'     ,/,         VAX. 902
     2         ' but  "STATISTICS"  in columns 55-64 of the',           VAX. 903
     3         ' last-read data card has been misspelled.'  ,/,         VAX. 904
     4         ' The defective 80-column card image =',  A80  )         VAX. 905
      STOP                                                              VAX. 906
 3920 IF ( BUS3 .NE. TEXT5 )   GO TO 3260                               M18. 340
      IF ( BUS4 .EQ. TEXT6 )   GO TO 3360                               M18. 341
 3260 IF ( BUS3 .NE. TEXT7 )   GO TO 605                                M18. 342
      IF ( BUS4 .NE. TEXT8 )   GO TO 605                                M13.1305
      SIGMAX = 0.0                                                      M13.1306
      IDIST = 0                                                         M13.1307
      IF (NENERG .LT. 0)  GO TO 615                                     M13.1308
      KILL = 141                                                        M13.1309
      LSTAT(19) = 3260                                                  M17. 379
      GO TO 9200                                                        M13.1311
  605 GO TO 209                                                         M32.1281
 3360 IF ( NENERG .GT. 0 ) GO TO 615                                    M36. 329
      KILL = 141                                                        M13.1313
      LSTAT(19) = 3360                                                  M17. 381
      GO TO 9200                                                        M13.1315
  615 AKEY(KSWTCH) = +44444.                                            M42.2931
      IF ( A  .NE.  3333. )   GO TO 3344                                M32.1282
      AKEY(KSWTCH) = -44444.                                            M32.1283
      NDX1 = LSWTCH + 1 - KSWTCH                                                
      IF ( CK1 .LE. 0.0 ) GO TO 4674                                            
C     PATCH TO ALLOW CURRENT MARGIN WITH RANDOM OPENING (SUBTS1):       M35. 849
      IF ( NDX1 .GT. KSWTCH )   GO TO 4668                                      
      WRITE (LUNIT6, 4661)  NDX1, KSWTCH, LSWTCH                        M35. 851
 4661 FORMAT (  37H OVERFLOW ERROR STOP, LIST 6.  RANDOM,               M35. 852
     1          29H OPENING WITH CURRENT MARGIN.,                       M35. 853
     2          25H   NDX1, KSWTCH, LSWTCH =,  3I8  )                   M35. 854
      CALL STOPTP                                                               
 4668 CRIT(NDX1) = CK1                                                          
      IF ( IPRSUP .GE. 1 )  WRITE (LUNIT6, 4669) I, KSWTCH, CK1         M36. 330
 4669 FORMAT ( 5X, 16HI, KSWTCH, CK1 =,  2I8, E15.5  )                  M35. 858
 4674 CONTINUE                                                                  
      TOPEN(KSWTCH) = TCLOSE(KSWTCH)                                    M32.1284
      TCLOSE(KSWTCH) = -1.0                                             M32.1285
 3344 KPASS = KPASS + 1                                                 M32.1286
      IF (BUS5 .NE. TEXT17)   GO TO 34605                               M13.1316
      KLOAEP = KSWTCH                                                   M13.1317
      GO TO 225                                                         M13.1318
34605 N5 = 0                                                            M13.1319
      N6 = 0                                                            M13.1320
      DO 240  I = 1, NTOT                                               M13.1321
      IF (BUS5 .EQ. BUS(I))  N5 = I                                     M13.1322
      IF (BUS6 .EQ. BUS(I))  N6 = I                                     M13.1323
  240 CONTINUE                                                          M13.1324
      IF (N5*N6 .EQ. 1)   GO TO 225                                     M13.1325
  241 N7 = KSWTCH - 1                                                   M13.1330
      IF ( N7  .LE.  0 )   GO TO 7245                                   M28.3863
      DO 245  I = 1, N7                                                 M13.1331
      IF (N5 .NE.  KMSWIT(I))  GO TO 245                                M32.1287
      NDX3 = LSWTCH + I                                                 M32.1288
      IF (N6 .NE.  KMSWIT(NDX3))  GO TO 245                             M32.1289
      KDEPSW(KSWTCH) = I                                                M13.1334
      GO TO 225                                                         M13.1335
  245 CONTINUE                                                          M13.1336
 7245 KILL = 110                                                        M28.3864
      LSTAT(19) = 245                                                   M13.1338
      LSTAT(15) = N5                                                    M13.1339
      LSTAT(16) = N6                                                    M13.1340
      GO TO 9200                                                        M13.1341
  225 TDNS(KSWTCH) = CK1                                                M23. 994
      TSTAT(KSWTCH) = GUS3                                              M05. 371
      IF ( GUS4 .LT. 0.0 )  GO TO  620                                  M32.1290
      NDXI = KSWTCH + LSWTCH                                            M32.1291
      TOPEN( NDXI ) = GUS4                                              M32.1292
      IF (IT2 .EQ. 9976 .OR. IDIST .EQ. 1)  TOPEN(NDXI) = - GUS4        M33. 215
      GO TO 209                                                         M42.2932
  617 KILL = 94                                                         M01.  15
      FLSTAT(14) = STSTAT                                               M15. 580
      FLSTAT(15) = SSIGMA                                               M15. 581
      GO TO 9200                                                        M01.  18
  618 KILL = 95                                                         M01.  19
      FLSTAT(14) = STSTAT                                               M15. 582
      FLSTAT(15) = SSIGMA                                               M15. 583
      GO TO 9200                                                        M01.  22
  620 KILL = 89                                                         M42.2933
      LSTAT(19) = 620                                                   M42.2934
      FLSTAT(15) = TSTAT(KSWTCH)                                        M42.2935
      FLSTAT(16) = GUS4                                                 M32.1298
      GO TO 9200                                                        M42.2936
  213 IF ( NENERG  .EQ.  0 )  GO TO 8299                                M33. 216
      DO 8211 I = 1, KSWTCH                                             M35. 860
      IF ( ABSZ ( AKEY(I) )  .NE.  44444. )  GO TO 8211                 M33. 218
      J = I                                                             M33. 219
      STSTAT = 0.0                                                      M33. 220
      SSIGMA = 0.0                                                      M33. 221
 4632 NDXJ = J + LSWTCH                                                 M33. 222
      TARG = TSTAT(J)                                                   M33. 223
      JK = KDEPSW(J)                                                    M33. 224
      IF ( KLOAEP .NE. 0 .AND. JK .EQ. 0 )                              M33. 225
     1 TARG = TSTAT(KLOAEP)                                             VAX. 907
      STSTAT = STSTAT + TARG                                            M33. 226
      SSIGMA = SQRTZ ( SSIGMA*SSIGMA +                                  M33. 227
     1 TOPEN(NDXJ) * TOPEN(NDXJ) )                                      VAX. 908
      IF (JK .EQ. 0)  GO TO 4682                                        M33. 228
      J = JK                                                            M33. 229
      GO TO 4632                                                        M33. 230
 4682 IF ( STSTAT .GE. 0.0 )   GO TO 4688                               M33. 231
      LSTAT(14) = I                                                     M35. 861
      LSTAT(15) = 4433                                                  M35. 862
      GO TO 620                                                         M33. 233
 4688 IF ( NENERG  .LT.  0 )  GO TO 8222                                M33. 234
      IF ( IDIST .EQ. 1  .OR.                                           M33. 235
     1 KSWTYP(I) .EQ. 9976 )  GO TO 616                                 VAX. 909
      TIMCHK = STSTAT -SIGMAX * SSIGMA                                  VAX. 910
      IF ( TIMCHK .LT. 0.0 ) GO TO 617                                  M33. 239
      GO TO 614                                                         M33. 240
 8222 ABC = 1.0 - ITEST                                                 M33. 241
      IF ( KLOAEP .EQ. 0                                                M35. 863
     1     .OR.  KLOAEP .EQ. I )  GO TO 8303                            VAX. 911
      NDXK = LSWTCH + KLOAEP                                            M35. 864
      STSTAT = STSTAT - 0.5 * ABC *                                     M35. 865
     1 TOPEN(NDXK) * (TDNS(KLOAEP) - 1.)                                VAX. 912
      ABC = 1.0                                                         M35. 866
 8303 TIMCHK = STSTAT - 0.5 * ABC *                                     M35. 867
     1 TOPEN(NDXJ) * (TDNS(J) - 1.)                                     VAX. 913
      IF ( TIMCHK  .LT.  0. )  GO TO 618                                M33. 243
      GO TO 614                                                         M33. 244
  616 TIMCHK = STSTAT - 1.732050808 * SSIGMA                            M33. 245
      IF (TIMCHK .LT. 0.0) GO TO 618                                    M33. 246
  614 IF ( TIMCHK  .LT.  TENERG )  TENERG = TIMCHK                      M33. 247
 8211 CONTINUE                                                          M33. 248
 8299 IF ( NOUTPR  .EQ.  0 )                                            M33. 249
     1 WRITE (KUNIT6, 54133)                                            M35. 868
54133 FORMAT (  '+BLANK CARD TERMINATING SWITCH CARDS.'  )              M42.2937
      CALL MOVE0 ( IPNTV(1), LL10 )                                     M36. 331
      IF ( TENERG  .GE.  1.E+19 )   GO TO 1642                          M27. 666
      IF ( TENERG  .LT.  TMAX )     GO TO 1642                          M23.1000
      IF ( NENERG  .EQ.  1 )        GO TO 1642                          M23.1001
      KILL = 207                                                        M23.1002
      LSTAT(19) = 1642                                                  M23.1003
      GO TO 9200                                                        M23.1004
 1642 NUMREF = 0                                                        M23.1005
      IF ( NENERG  .EQ.  0 )   GO TO 640                                M13.1358
      N1 = 1                                                            M13.1359
      DO 625  I = 1, KSWTCH                                             M13.1360
      IF ( ABSZ ( AKEY(I) )   .NE.   44444. )   GO TO 625               M24. 616
      IF (KDEPSW(I) .NE. 0)   GO TO 625                                 M13.1362
      NUMREF = NUMREF + 1                                               M13.1363
      IF (NUMREF .LE. 10)   GO TO 500                                   M13.1364
      KILL = 112                                                        M13.1365
      LSTAT(14) = I                                                     M13.1366
      LSTAT(19) = 500                                                   M18. 347
      N1 = KMSWIT(I)                                                    M32.1300
      NDX4 = LSWTCH + I                                                 M32.1301
      N2 = KMSWIT(NDX4)                                                 M32.1302
      BUS1 = BUS(N1)                                                    M13.1370
      BUS2 = BUS(N2)                                                    M13.1371
      GO TO 9200                                                        M13.1372
  500 IF (NENERG .GT. 0)   GO TO 510                                    M13.1373
      N1 = N1 *TDNS(I)                                                  M13.1374
  510 IPNTV(NUMREF) = I                                                 M36. 332
      IF (KLOAEP .NE. I )   GO TO  625                                  M13.1376
      ILOAEP = NUMREF                                                   M13.1377
  625 CONTINUE                                                          M13.1378
      IF ( NENERG .GT. 0 )   GO TO 640                                  M36. 333
      N2 = IABS(NENERG)                                                 M13.1380
      IF (N1 .EQ. N2)   GO TO 640                                       M13.1381
      KILL = 113                                                        M13.1382
      LSTAT(19) = 625                                                   M13.1383
      LSTAT(12) = N1                                                    M35. 869
      J = 0                                                             M13.1384
      DO  520   I = 1, KSWTCH                                           M13.1385
      IF ( ABSZ (AKEY(I) ) .NE. 44444.)   GO TO 520                     M24. 617
      IF ( KDEPSW(I) .NE. 0 )   GO TO 520                               M35. 870
      J = J+1                                                           M13.1388
      IPNTV(J) = TDNS(I)                                                M13.1389
  520 CONTINUE                                                          M13.1390
      GO TO 9200                                                        M13.1391
  640 IF (NENERG .LE. 0)   GO TO 643                                    M35. 871
      IF ( ISW  .NE.  4444 )   GO TO 643                                M14.1284
      CALL RUNTYM ( D1, D2 )                                            M21.  31
      FLSTAT(1) = FLSTAT(1) + D1                                        M21.  32
      FLSTAT(2) = FLSTAT(2) + D2                                        M21.  33
      FLSTAT(5) = FLSTAT(5) - D1                                        M21.  34
      FLSTAT(6) = FLSTAT(6) - D2                                        M21.  35
      LASTOV = NCHAIN                                                   M15. 584
      NCHAIN = 12                                                       M14.1285
      IF ( IPRSUP  .GE.  1 )                                            M32.1304
     1 WRITE ( LUNIT6, 4568 )                                           M32.1305
 4568 FORMAT ( 23H  "EXIT  MODULE OVER5." )                             M32.1306
      GO TO 99999                                                       M14.1286
  643 LSTAT(21) = NTOT                                                  M32.1307
      LSTAT(26) = KSWTCH                                                M42.2938
      LSTAT(38) = NPOWER                                                M42.2939
      IF ( IPRSUP .GE. 2 )                                              M42.2940
     1 WRITE(LUNIT6, 8025)  IBR, INONL, KSWTCH, NV, NPOWER              M33. 250
     1 , NTOT, IT, IFDEP, NUM99, ICHAR, KNT                             M42.2941
 8025 FORMAT ( /,  59H MISC. SCALARS, IN  'OVER5'  AFTER COMPLETING SWITM42.2942
     1CH INPUT.   ,/,  1X, 88H     IBR   INONL  KSWTCH      NV  NPOWER  M33. 251
     2  NTOT      IT   IFDEP   NUM99   ICHAR     KNT   ,/, 1X, 11I8 )   M33. 252
      IF ( IPRSUP .GE. 2 )                                              M42.2943
     1 WRITE(LUNIT6, 8026)  ( KOUTVP(I), I = 1, NPOWER )                M35. 872
 8026 FORMAT ( /, 15H KOUTVP, KOUTIE   ,/, ( 1X, 10I10 ) )              M42.2944
      NDX1 = MAXPE + 1                                                  M35. 873
      NDX2 = MAXPE + NPOWER                                             M35. 874
      IF ( IPRSUP .GE. 2 )                                              M35. 875
     1 WRITE(LUNIT6, 8026)  ( KOUTVP(I), I=NDX1, NDX2 )                 M35. 876
      IPUNCH = IPUN                                                     M42.2945
      IOUT = IOUTIN                                                     M42.2946
C     CODE FOR INPUT OF EMTP SOURCES IS IN  "OVER5A" .                  M28.3865
      CALL OVER5A                                                       M28.3866
      IF ( KTAB .EQ. 0 ) GO TO 3636                                     M39.1027
      NDX1 = KXTCS + NUK                                                M39.1028
      XTCS(NDX1+5) = OMEGA                                              M39.1029
      XTCS(NDX1+4) = OMEGA / TWOPI                                      M39.1030
 3636 IF ( IPRSUP  .GE.  1 )                                            M39.1031
     1 WRITE ( LUNIT6, 4568 )                                           M32.1309
      IF ( KILL  .EQ.  0 )   GO TO 99999                                M28.3867
      GO TO 9200                                                        M28.3868
 9000 LSTAT(16) = IPRINT                                                M28.3869
      KILL = 1                                                          M28.3870
 9200 LASTOV = NCHAIN                                                   M28.3871
      NCHAIN = 51                                                       M28.3872
      LSTAT(18) = 5                                                     M28.3873
      IF ( IPRSUP  .GE.  1 )                                            M32.1310
     1 WRITE ( LUNIT6, 4568 )                                           M32.1311
99999 RETURN                                                            M28.3874
      END                                                               M28.3875
      SUBROUTINE OVER5A                                                 M28.3876
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'SYNMAC.FOR;/NOLIST'                                           
      INCLUDE  'SYNCOM.FOR;/NOLIST'                                           
      INCLUDE  'UMDECK.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      DIMENSION  ISPUM(1), KPEN(4)                                      M39.1032
      EQUIVALENCE  ( SPUM(1), ISPUM(1) )                                M32.1313
      REAL*8           TEXT12                                           M28.3883
      DATA  TEXT12   / 6HTYP-16 /                                       M28.3884
      LL2 = 2                                                           M35. 878
      LL3 = 3                                                           M35. 879
      LL4 = 4                                                           M35. 880
      IREAD=0                                                           M42.2947
      ISTEAD=0                                                          M42.2948
      IPRINT=4                                                          M42.2949
      KPU=0                                                             M42.2950
      NSMOUT = 0                                                        M16. 588
      MACHFL = 0                                                        M16. 591
      NUMUM = 0                                                         M37.2457
      CALL MOVE0 ( KSMSPY(1), LL3 )                                             
      CALL MOVE0 ( KODE(1), NTOT )                                      M32.1314
      KODE(1) = -1                                                      M32.1315
C     READ INPUT CARD USING CIMAGE.                                     M42.2951
  310 CALL CIMAGE                                                       M42.2952
      IF ( KILL  .GT.  0 )   RETURN                                     M28.3885
      IF ( MACHFL.EQ.1 ) GO TO 110                                      M16. 592
      IF ( MACHFL.EQ.2 ) GO TO 110                                      M16. 593
      IF ( MACHFL.EQ.3 ) MACHFL=0                                       M16. 594
      IF ( KOLBEG  .GT.  0 )   GO TO 313                                M20.1086
      DECODE (80, 6304, ABUFF)  N2, BUS1, N1                            M32.1316
 6304 FORMAT ( I2, A6, I2 )                                             M32.1317
      IF ( N2  .NE.  18 )   GO TO 6347                                  M32.1318
C     SPECIAL TYPE-18 INPUT EXTENDS PRECEDING SOURCE TO ALLOW           M32.1319
C     FOR UNGROUNDED 2ND TERMINAL AND IDEAL TRANSFORMER:                M32.1320
      KCONST = KCONST + 1                                               M32.1321
      LSTAT(19) = 6304                                                  M32.1322
      IPRINT = 4                                                        M32.1323
      IF ( KCONST  .GT.  LEXCT )   GO TO 9000                           M32.1324
      DECODE (80, 6309, ABUFF)  CREST(KCONST), BUS2, BUS3, BUS4         M32.1325
 6309 FORMAT ( 10X,  E10.0,  3A6  )                                     M32.1326
      CALL MOVE0 ( KPEN(1), LL4 )                                       M39.1033
      DO 6322  J=1, NTOT                                                M32.1328
      IF ( BUS(J) .NE. BUS1 )  GO TO 6313                               M32.1329
      KPEN(1) = J                                                       M32.1330
      NODE(KCONST) = J                                                  M32.1331
      IFORM(KCONST) = 18                                                M32.1332
 6313 IF ( BUS(J) .NE. BUS2 )  GO TO 6317                               M32.1333
      KPEN(2) = J                                                       M32.1334
      TIME2(KCONST) = J                                                 M32.1335
 6317 IF ( BUS(J) .NE. BUS3 )  GO TO 6322                               M32.1336
      KPEN(3) = J                                                       M32.1337
      SFREQ(KCONST) = J                                                 M32.1338
 6322 CONTINUE                                                          M32.1339
      IF ( KPEN(1)  .NE.  0 )  GO TO 6325                               M32.1340
      BUS6 = BUS1                                                       M32.1341
      GO TO 6330                                                        M32.1342
 6325 IF ( KPEN(2)  .NE.  0 )  GO TO 6326                               M32.1343
      BUS6 = BUS2                                                       M32.1344
      GO TO 6330                                                        M32.1345
 6326 IF ( KPEN(3)  .NE.  0 )  GO TO 6335                               M32.1346
      BUS6 = BUS3                                                       M32.1347
 6330 WRITE (LUNIT6, 6331)  BUS6                                        M32.1348
 6331 FORMAT ( 35H  ++++++  WARNING.  ++++++  TYPE-18,                  M32.1349
     1         39H SOURCE HAS UNRECOGNIZABLE NODE NAME  ",              M32.1350
     2  A6,    21H" .   DEVICE IGNORED.     )                           M32.1351
      KCONST = KCONST - 1                                               M32.1352
      GO TO 310                                                         M32.1353
 6335 IF ( N1  .NE.  -1 )   GO TO 6339                                  M32.1354
C     UNGROUNDED CURRENT SOURCE (NO TRANSFORMER ALLOWED):               M32.1355
      IFORM(KCONST) = IFORM(KCONST-1)                                   M32.1356
      CREST(KCONST) = -CREST(KCONST-1)                                  M32.1357
      NODE(KCONST) = KPEN(1)                                            M32.1358
      SFREQ(KCONST) = SFREQ(KCONST-1)                                   M32.1359
      TSTART(KCONST) = TSTART(KCONST-1)                                 M32.1360
      TSTOP(KCONST) = TSTOP(KCONST-1)                                   M32.1361
      TIME1(KCONST) = TIME1(KCONST-1)                                   M32.1362
      GO TO 310                                                         M32.1363
 6339 N14 = IABS ( NODE(KCONST-1) )                                     M32.1364
      KODE(N14) = 0                                                     M32.1365
      NTOT = NTOT + 1                                                   M32.1366
      KTRANS(NTOT)=-664422                                              VAX. 914
      IPRINT = 1                                                        M32.1367
      LSTAT(19) = 6335                                                  M32.1368
      IF ( NTOT  .GT.  LBUS )   GO TO 9000                              M32.1369
      BUS(NTOT) = BUS4                                                  M32.1370
      TIME1(KCONST) = NTOT                                              M32.1371
      IPRINT = 2                                                        M32.1372
      LSTAT(19) = 6343                                                  M32.1373
      IF ( IBR+8  .GT.  LBRNCH )   GO TO 9000                           M32.1374
      IPRINT = 3                                                        M32.1375
      LSTAT(19) = 6343                                                  M32.1376
      IF ( IT+8  .GT.  LDATA )   GO TO 9000                             M33. 253
      KPEN(4) = IABS ( NODE(KCONST-1) )                                 M32.1378
      VOLTI(LL4) = -1.0                                                 M35. 881
      VOLTI(1) = +1.0                                                   M32.1380
      VOLTI(LL2) = +1.0 / CREST(KCONST)                                 M35. 882
      VOLTI(LL3) = - VOLTI(LL2)                                         M35. 883
      DO 6343  J=1, 4                                                   M32.1383
      IF ( VOLTI(J)  .EQ.  0.0 )   GO TO 6343                           M32.1384
      IBR = IBR + 1                                                     M32.1385
      KBUS(IBR) = KPEN(J)                                               M32.1386
      MBUS(IBR) = NTOT                                                  M32.1387
      LENGTH(IBR) = 1                                                   M32.1388
      NR(IBR) = IT + 1                                                  M32.1389
      IT = IT + 1                                                       M32.1390
      TR(IT) = VOLTI(J)                                                 M32.1391
      TX(IT) = 0.0                                                      M32.1392
      C(IT)  = 0.0                                                      M32.1393
      R(IT)  = 0.0                                                      M32.1394
      IBR = IBR + 1                                                     M32.1395
      KBUS(IBR) = KPEN(J)                                               M32.1396
      MBUS(IBR) = 1                                                     M32.1397
      LENGTH(IBR) = 1                                                   M32.1398
      NR(IBR) = IT + 1                                                  M32.1399
      IT = IT + 1                                                       M32.1400
      TR(IT) = -VOLTI(J)                                                M32.1401
      TX(IT) = 0.0                                                      M32.1402
      C(IT) = 0.0                                                       M32.1403
      R(IT) = 0.0                                                       M32.1404
 6343 CONTINUE                                                          M32.1405
      GO TO 310                                                         M32.1406
C     END OF TYPE-18 (IDEAL TRANSFORMER) CODE;  CONSIDER NON-18:        M32.1407
 6347 CONTINUE                                                          M32.1408
      DECODE (80, 6358, ABUFF)  A, D1, GUS2, H1, H2, GUS3, GUS4         M32.1409
 6358 FORMAT ( 10X, 7E10.0 )                                            M32.1410
      IF ( N2  .NE.  19 )   GO TO 314                                   M27. 667
C     UNIVERSAL MACHINE OF HIAN LAUW --- TYPE-19 SOURCE.                M27. 668
      CALL UMOFFS                                                       M27. 669
      CALL UMDATA ( SPUM(IUREAC),                                       M33. 254
     1 SPUM(IUGPAR), SPUM(IUFPAR), SPUM(IUHIST), SPUM(IUUMRP),          M33. 255
     2 ISPUM(IUNOD1), ISPUM(IUNOD2), ISPUM(IUJCLT), ISPUM(IUJCLO),      M32.1411
     3 ISPUM(IUJTYP), ISPUM(IUNODO), ISPUM(IUJTMT), SPUM(IUHISM),       M32.1412
     4 SPUM(IUOMGM), SPUM(IUOMLD), SPUM(IUTHAM), SPUM(IUREDU),          M27. 674
     5 SPUM(IUREDS), SPUM(IUFLDS), SPUM(IUFLDR), SPUM(IUREQU),          M27. 675
     6 SPUM(IUFLQS), SPUM(IUFLQR), ISPUM(IUJCDS), ISPUM(IUJCQS),        M32.1413
     7 SPUM(IUFLXD), SPUM(IUFLXQ), ISPUM(IUNPPA), SPUM(IUROTM),         M32.1414
     8 ISPUM(IUNCLD), ISPUM(IUNCLQ), ISPUM(IUJTQO), ISPUM(IUJOMO),      M32.1415
     9 ISPUM(IUJTHO), SPUM(IUREQS), SPUM(IUEPSO), SPUM(IUDCOE),         M32.1416
     9 ISPUM(IUKCOI), SPUM(IUVOLT), SPUM(IUANGL), ISPUM(IUNODF),        M32.1417
     9 ISPUM(IUNODM), ISPUM(IUKUMO), ISPUM(IUJUMO), SPUM(IUUMOU))       M32.1418
      IF ( KILL  .GT.  0 )   RETURN                                     M28.3886
      GO TO 310                                                         M27. 682
  313 CONTINUE                                                          M20.1088
      DECODE (80, 6304, ABUFF(1) )   N2, BUS1                           M32.1419
      NFRFLD = 1                                                        M20.1090
      NRIGHT = 6                                                        M20.1091
      CALL FREONE ( H2 )                                                M29.1406
      N1 = H2                                                           M28.3888
      CALL FREONE ( A )                                                 M29.1407
      CALL FREONE ( D1 )                                                M29.1408
      CALL FREONE ( GUS2 )                                              M29.1409
      CALL FREONE ( H1 )                                                M29.1410
      CALL FREONE ( H2 )                                                M29.1411
      CALL FREONE ( GUS3 )                                              M29.1412
      CALL FREONE ( GUS4 )                                              M29.1413
  314 SMANG = GUS2                                                      M20.1100
      IF ( N2  .GT.  0 )   GO TO 4568                                   M22.2743
      IF ( BUS1  .NE.  BLANK )   GO TO 4562                             M22.2744
      IF ( A  .EQ.  0.0 )   GO TO 400                                   M22.2745
 4562 KILL = 10                                                         M22.2746
      LSTAT(19) = 4562                                                  M22.2747
      LSTAT(16) = N2                                                    M22.2748
      RETURN                                                            M28.3889
 4568 IF ( N2  .GE.  50     .AND.                                       M22.2750
     1     N2  .LE.  59 )   GO TO 114                                   M22.2751
C     CHECK THAT CONVENTIONAL SOURCES DONT FOLLOW SYNCH.MACHINES        M16. 597
      IF(NUMSM .EQ. 0) GO TO 114                                        M16. 598
      KILL= 177                                                         M16. 599
      LSTAT(14) = N2                                                    M16. 600
      LSTAT(19)= 8                                                      M16. 601
      RETURN                                                            M28.3890
  110 IF ( KOLBEG  .GT.  0 )   GO TO 7642                               M22.2752
      DECODE (80, 111, ABUFF(1) )  BUS1                                 M16. 604
  111 FORMAT(2X,A6)                                                     M16. 605
      GO TO 7646                                                        M22.2753
 7642 NFRFLD = 1                                                        M22.2754
      NRIGHT = -1                                                       M22.2755
      CALL FREONE ( D1 )                                                M29.1414
      NRIGHT = 0                                                        M22.2757
      BUS1 = TEXTA6(1)                                                  M22.2758
 7646 SMANG = SMANG - 120.                                              M22.2759
      DECODE(80,102,ABUFF(1) ) SMAMP, SMANGL                            M32.1421
 102  FORMAT(10X,E10.6,10X,E10.6)                                       M32.1422
      IF( SMAMP .EQ. 0.  .AND. SMANGL .EQ. 0.  )  GO  TO  103           M35. 884
      SMANG = SMANGL                                                    M32.1424
      A = SMAMP                                                         M32.1425
 103  N2 = 14                                                           M32.1426
      GUS2=SMANG                                                        M16. 608
      IF ( MACHFL  .EQ.  2 )   GO TO 130                                M16. 609
      IF ( NOUTPR  .EQ.  0 )                                            M20.1101
     1 WRITE (KUNIT6, 3128)  A, SMANG                                   M35. 885
 3128 FORMAT( 19H+2ND PHASE OF S.M. , E12.5, 2X, F8.2 )                 M32.1428
      GO TO 113                                                         M16. 612
  130 IF ( NOUTPR  .EQ.  0 )                                            M20.1103
     1 WRITE (KUNIT6, 3129)  A, SMANG                                   M35. 886
 3129 FORMAT( 19H+3RD PHASE OF S.M. , E12.5, 2X, F8.2 )                 M32.1430
C     TRANSFER CONTROL TO SYNCHRONOUS MACHINE DATA-INPUT SUBROUTINE.    M16. 615
      CALL SMDAT ( N2MACH )                                             M35. 887
      IF ( KILL  .GT.  0 )   RETURN                                     M28.3891
 113  MACHFL = MACHFL + 1                                               M38.1122
 114  KCONST = KCONST + 1                                               M38.1123
      KPU = KPU+1                                                       M42.2953
      IF(N2 .LT. 50 ) GO TO 115                                         M16. 622
      IF(N2 .GT. 59) GO TO 115                                          M16. 623
C     THIS SOURCE IS PHASE A OF A SYNCHRONOUS MACHINE                   M16. 624
      NUMSM = NUMSM + 1                                                         
      J30 = 30 * NUMSM - 28                                                     
      IF(NUMSM.LE. LSYN) GO TO 125                                      M16. 627
      KILL= 1                                                           M16. 628
      LSTAT(16) = 17                                                    M16. 629
      LSTAT(19)=125                                                     M16. 630
      RETURN                                                            M28.3893
 125  MACHFL = 1                                                                
      N2MACH=N2                                                         M16. 633
      N2=14                                                             M16. 634
      GUS3 = -9988.                                                     M19. 374
  115 IF( KCONST .LE. LEXCT )  GO TO 27                                 M16. 635
      LSTAT(19) = 310                                                   M42.2954
      GO TO 9000                                                        M42.2955
   27 IF( GUS4 .EQ. 0.0 )  GUS4 = FLTINF                                M05. 374
      IF (KPU .EQ. 1)                                                   M13.1393
     1 PU = A                                                           M13.1394
      SFREQ(KCONST) = D1                                                M42.2956
      IF ( MACHFL  .LE.  1      .AND.                                   M20.1119
     1     NOUTPR  .EQ.  0 )                                            M20.1120
     1 WRITE (KUNIT6, 7)  A, SFREQ(KCONST), GUS2, GUS3                  M35. 888
    7 FORMAT ( 8H+SOURCE., 2X, 4E10.2  )                                M42.2957
      IF ( N2  .EQ.  17 )  GO TO 4259                                   M32.1431
      IF ( N2  .NE.  14 )   GO TO 317                                   M15. 585
      IF ( SFREQ(KCONST)  .GT.  0.0 )   GO TO 317                       M15. 586
      KILL = 149                                                        M15. 587
      FLSTAT(14) = SFREQ(KCONST)                                        M15. 588
      LSTAT(19) = 317                                                   M15. 589
      RETURN                                                            M28.3894
  317 DO 320  I=1, NTOT                                                 M15. 591
      IF( BUS1 .EQ. BUS(I) )  GO TO 330                                 M42.2958
  320 CONTINUE                                                          M42.2959
      LSTAT(19) = 320                                                   M42.2960
 4258 KILL = 12                                                         M42.2961
      RETURN                                                            M28.3895
  330 IF ( N2  .LE.  16 )   GO TO 4250                                  M32.1432
      LSTAT(19) = 330                                                   M42.2962
      IF ( N2  .LT.  60 )   GO TO 4249                                  M21.  36
 4259 DO 3429  M=1, KTAB                                                M32.1433
      NDX1 = ILNTAB( KLNTAB + M )                                       M38.1127
      IF ( BUS1  .EQ.  TEXVEC(NDX1) )   GO TO 3435                      M38.1128
 3429 CONTINUE                                                          M21.  39
      LSTAT(19) = 3429                                                  M21.  40
      GO TO 4249                                                        M21.  41
 3435 SFREQ(KCONST) = M                                                 M21.  42
      GO TO 362                                                         M21.  43
 4249 KILL = 10                                                         M42.2963
      LSTAT(16) = N2                                                    M42.2964
      RETURN                                                            M28.3896
 4250 IF( N2 .NE. 16 )  GO TO 349                                       M42.2965
      IF( N1 .GE. 1  .AND.  N1 .LE. 3 )  GO TO 4254                     M42.2966
      KILL = 11                                                         M42.2967
      LSTAT(19) = 4250                                                  M42.2968
      LSTAT(16) = N1                                                    M42.2969
      RETURN                                                            M28.3899
 4254 D2 = GUS2 + H2                                                    M05. 376
      H3=GUS2*H2                                                        M05. 377
      YX=GUS3                                                           M05. 378
      GUS3=1.0+D2/DELTA2+H3/DELTA2/DELTA2                               M05. 379
      GUS4=(1.0+H1/DELTA2)*A                                            M05. 380
      CK1=SFREQ(KCONST)                                                 M42.2970
      SFREQ(KCONST)=A                                                   M42.2971
      TIME1(KCONST) = GUS3                                              M05. 381
      TSTART(KCONST)=4.0*H3/DELTAT/GUS3                                 M05. 382
      IFORM(KCONST)=16                                                  M42.2972
      NODE(KCONST)  =-I                                                 M42.2973
      KCONST=KCONST+1                                                   M42.2974
      LSTAT(19)= 4254                                                   M42.2975
      IF(KCONST.GT.LEXCT) GO TO 9000                                    M42.2976
      IFORM(KCONST)=N1                                                  M42.2977
C     READ INPUT CARD USING CIMAGE.                                     M42.2978
      CALL CIMAGE                                                       M42.2979
      IF ( KOLBEG  .GT.  0 )   GO TO 7653                               M22.2762
      DECODE (80, 4255, ABUFF(1) )   BUS1, N3, A, GUS2,                 M32.1434
     1                               TIME1(KCONST), TSTART(KCONST),     M12. 300
     2                               REPS, DCFREQ, LOUTBR               M28.3900
 4255 FORMAT ( 2X, A6, I2, 6E10.6,  9X, I1 )                            M32.1435
      GO TO 7658                                                        M22.2763
 7653 NFRFLD = 1                                                        M22.2764
      CALL FREFLD ( VOLTBC(1) )                                         M31. 791
      N3 = VOLTBC(1)                                                    M31. 792
      NRIGHT = -1                                                       M22.2767
      CALL FREONE ( D1 )                                                M29.1415
      NRIGHT = 0                                                        M22.2769
      BUS1 = TEXTA6(1)                                                  M22.2770
      CALL FREFLD ( VOLTBC(1) )                                         M31. 793
      N3 = VOLTBC(1)                                                    M31. 794
      CALL FREONE ( A )                                                 M29.1416
      CALL FREONE ( GUS2 )                                              M29.1417
      CALL FREFLD ( TIME1(KCONST) )                                     M22.2775
      CALL FREFLD ( TSTART(KCONST) )                                    M22.2776
      CALL FREONE ( REPS )                                              M29.1418
      CALL FREONE ( DCFREQ )                                            M29.1419
      CALL FREFLD ( VOLTBC(1) )                                         M31. 795
      LOUTBR = VOLTBC(1)                                                M31. 796
 7658 IF ( NOUTPR  .EQ.  0 )                                            M22.2780
     1 WRITE (KUNIT6, 54137)                                            M35. 889
54137 FORMAT(  26H+SECOND DC SIMULATOR CARD.  )                         M42.2980
      IF ( DCFREQ .EQ. 0.) DCFREQ = TENM3                               M28.3903
      CREST(KCONST-1)=A*GUS3/GUS2/GUS4                                  M05. 383
      GUS3=GUS4*GUS2/GUS3                                               M05. 384
      DO 341 K=1,NTOT                                                   M42.2981
      IF(BUS1.EQ.BUS(K))GO TO 342                                       M42.2982
  341 CONTINUE                                                          M42.2983
      LSTAT(19) = 341                                                   M42.2984
      GO TO 4258                                                        M42.2985
  342 N3=4                                                              M42.2986
      IF(YX.LT.0.) YX=0.                                                M42.2987
      NTOT = NTOT + 1                                                   M12. 303
      BUS(NTOT) = TEXT12                                                M12. 304
      KODE(NTOT) = 0                                                    M32.1436
      IF ( LOUTBR  .LT.  2 )   GO TO 4257                               M12. 306
      NV = NV + 1                                                       M12. 307
      IF ( NV  .LE.  LSIZ12 )   GO TO 4256                              M38.1129
      IPRINT = 11                                                       M12. 309
      LSTAT(19) = 4256                                                  M12. 310
      GO TO 9000                                                        M12. 311
 4256 IBRNCH(NV) = K                                                    M12. 312
      JBRNCH(NV) = I                                                    M12. 313
      IF ( LOUTBR  .EQ.  2 )   LOUTBR = 0                               M12. 314
 4257 NTOT = NTOT + 1                                                   M12. 315
      IPRINT=1                                                          M42.2988
      LSTAT(19)=  342                                                   M42.2989
      IF(NTOT.GT.LBUS) GO TO 9000                                       M42.2990
      BUS(NTOT)=TRASH                                                   M42.2991
      KODE(NTOT) = 0                                                    M32.1437
      IPRINT=6                                                          M42.2992
      KSWTCH=KSWTCH+1                                                   M42.2993
      IF( KSWTCH .LE. LSWTCH )  GO TO 2342                              M42.2994
      LSTAT(19) = 2342                                                  M42.2995
      GO TO 9000                                                        M42.2996
 2342 KMSWIT(KSWTCH) = NTOT                                             M32.1438
      NDX1 = KSWTCH + LSWTCH                                            M32.1439
      KMSWIT(NDX1) = NTOT - 1                                           M32.1440
      KPOS(KSWTCH) = N3                                                 M42.2997
      KSWTYP(KSWTCH) = 0                                                M32.1441
      TCLOSE(KSWTCH)=-YX                                                M42.2998
      TOPEN(KSWTCH)= DELTAT +  DELTAT / 2.0                             M05. 385
      ADELAY(KSWTCH)=0.                                                 M42.2999
      CRIT(KSWTCH)=0.                                                   M42.3000
      ENERGY(KSWTCH)=0.                                                 M42.3001
      ISOURC(KSWTCH)=0                                                  M42.3002
      IBR = IBR + 1                                                     M12. 317
      KBUS(IBR) = K                                                     M12. 318
      MBUS(IBR) = NTOT - 1                                              M12. 319
      IF ( LOUTBR  .GT.  0 )   MBUS(IBR) = -MBUS(IBR)                   M12. 320
      LENGTH(IBR) = 1                                                   M12. 321
      IT = IT + 1                                                       M12. 322
      IF ( REPS  .EQ.  0.0 )   REPS = SQRTZ(EPSILN)                     M18. 348
      TR(IT) = REPS                                                     M12. 324
      TX(IT) = 0.0                                                      M12. 325
      C(IT) = 0.0                                                       M12. 326
      NR(IBR) = -IT                                                     M12. 327
      K=NTOT                                                            M42.3003
      NODE(KCONST)=-K                                                   M42.3004
      IBR=IBR+1                                                         M42.3005
      IPRINT=2                                                          M42.3006
      IF( IBR .LE. LBRNCH )  GO TO 2343                                 M42.3007
      LSTAT(19) = 2343                                                  M42.3008
      GO TO 9000                                                        M42.3009
 2343 KBUS(IBR) = I                                                     M42.3010
      MBUS(IBR)=K                                                       M42.3011
      IT=IT+1                                                           M42.3012
      IPRINT=3                                                          M42.3013
      IF( IT .LE. LDATA )  GO TO 2344                                   M42.3014
      LSTAT(19) = 2344                                                  M42.3015
      GO TO 9000                                                        M42.3016
 2344 IPRINT = 4                                                        M42.3017
      NR(IBR)=-IT                                                       M42.3018
      LENGTH(IBR)=1                                                     M42.3019
      C(IT)=0.                                                          M42.3020
      TR(IT)=GUS3                                                       M05. 386
      TX(IT)=0.                                                         M42.3021
      IF ( NOUTPR  .EQ.  0 )                                            M20.1124
     1 WRITE (KUNIT6, 340)  GUS3                                        M35. 890
  340 FORMAT ( 1H+, 30X, 8HEQUIV R=, F10.6  )                           M42.3022
      TSTOP(KCONST-1)=1.0/GUS3                                          M05. 388
      CREST(KCONST)=GUS4                                                M05. 389
      TSTOP(KCONST)=CK1                                                 M42.3023
      IF(N1.LE.0) GO TO 310                                             M42.3024
      KCONST=KCONST+2                                                   M42.3025
      LSTAT(19)=  340                                                   M42.3026
      IF(KCONST.GT.LEXCT) GO TO 9000                                    M42.3027
      IF ( ISTEAD .EQ. 0)   OMEGA = DCFREQ * TWOPI                      M28.3904
      ISTEAD=1                                                          M42.3028
      NODE(KCONST-1)=-I                                                 M42.3029
      NODE(KCONST)=-K                                                   M42.3030
      IFORM(KCONST-1) = 14                                              M42.3031
      IFORM(KCONST) = 14                                                M42.3032
      IF(N1.NE.1)GO TO 343                                              M42.3033
      IF( CK1  .LE.  TSTART(KCONST-2) )  GO TO 4265                     M42.3034
 4264 KILL = 13                                                         M42.3035
      LSTAT(19) = 4264                                                  M42.3036
      RETURN                                                            M28.3905
 4265 IF( CK1 .LT. TIME1(KCONST - 2 ) ) GO TO 4264                      M42.3037
  343 IF(N1.EQ.2) CK1=TSTART(KCONST-2)                                  M42.3038
      IF(N1 .EQ. 3 ) CK1 = TIME1(KCONST - 2 )                           M42.3039
       GUS2=CK1/GUS3+YX                                                 M05. 391
      CREST(KCONST-1)= GUS2                                             M05. 392
      CREST(KCONST)=-GUS2                                               M05. 393
      TSTART(KCONST - 1 ) = -1.0                                        M42.3040
      TSTART(KCONST) = -1.0                                             M42.3041
      TSTOP(KCONST - 1) = DELTA2                                        M42.3042
      TSTOP(KCONST) = DELTA2                                            M42.3043
      SFREQ(KCONST) = DCFREQ                                            M28.3906
      SFREQ(KCONST - 1 ) = DCFREQ                                       M28.3907
      TIME1(KCONST - 1 ) = 0.0                                          M42.3044
      TIME1(KCONST) = 0.0                                               M42.3045
      GO TO 310                                                         M42.3046
  349 IF( N2 .LE. 10 )  GO TO 362                                       M42.3047
      IF( A .NE. 0.0 )  GO TO 362                                       M42.3048
      KILL = 73                                                         M42.3049
      LSTAT(19) = 349                                                   M42.3050
      LSTAT(16) = N2                                                    M42.3051
      RETURN                                                            M28.3908
  362 IF( N1 .GE. 0 )  GO TO 336                                        M42.3052
      I=-I                                                              M42.3053
  331 NODE(KCONST)=I                                                    M42.3054
      K13 = J30 + MACHFL                                                M38.1130
      ISMDAT( K13 ) = I                                                         
      IF( N2 .GT. 0 )  GO TO 4266                                       M42.3055
      LSTAT(19) = 331                                                   M42.3056
      GO TO 4249                                                        M42.3057
 4266 TSTOP(KCONST) = GUS4                                              M05. 396
      IFORM(KCONST)=N2                                                  M42.3058
      CREST(KCONST)=A                                                   M42.3059
      IF(N2.EQ.14) GO TO 332                                            M42.3060
      IF(GUS3.LT.0.) GUS3=0.                                            M05. 397
      IF(N2.LE.10  .AND.  CREST(KCONST) .EQ. 0.0)IREAD=1                M35. 891
      IF(N2.EQ.13)SFREQ(KCONST)=(H1-A)/(H2-GUS2)                        M05. 398
      GO TO 333                                                         M42.3061
  332 GUS2 = GUS2 * TWOPI / 360.                                        M05. 399
      IF(H1.GT. 0.) GUS2=GUS2*SFREQ(KCONST)*360.0                       M05. 400
      IF(GUS3.GE.0.) GO TO 333                                          M05. 401
      IF( OMEGA .EQ. 0.0    .AND.    ISTEAD .EQ. 0 )                    M42.3062
     1 OMEGA = TWOPI * SFREQ(KCONST)                                    M42.3063
      ISTEAD=1                                                          M42.3064
  333 TIME1(KCONST) = GUS2                                              M05. 402
      TSTART(KCONST)=GUS3                                               M05. 403
      IF(I.LT.0) GO TO 310                                              M42.3065
      K=0                                                               M42.3066
  334 K=K+1                                                             M42.3067
      IF(K.EQ.KCONST) GO TO 310                                         M42.3068
      IF(NODE(K).EQ.I) GO TO 335                                        M42.3069
      GO TO 334                                                         M42.3070
  335 IFORM(KCONST)=-N2                                                 M42.3071
      GO TO 310                                                         M42.3072
  336 KODE(I) = -I                                                      M32.1442
      GO TO 331                                                         M42.3073
  400 IF ( NOUTPR  .EQ.  0 )                                            M20.1126
     1 WRITE (KUNIT6, 54139)                                            M35. 892
54139 FORMAT(  37H+BLANK CARD TERMINATING SOURCE CARDS.   )             M42.3074
      IF( NUMSM .GT. 0 )    MFIRST = ISMDAT( 24 )                               
      CALL INTERP                                                       M35. 893
      IF ( KCONST  .LE.  1 )   GO TO 4145                               M32.1443
      DO 4142  J=1, KCONST                                              M32.1444
      IF ( IABS ( IFORM(J) )  .NE.  17 )   GO TO 4142                   M32.1445
      IFORM(J) = 17                                                     M32.1446
      CREST(J) = CREST(J+1)                                             M32.1447
      WRITE (LUNIT6, 6616)  J, CREST(J)                                 M32.1448
 6616 FORMAT ( 32H TYPE-17 REASSIGN. J, CREST(J) =, I5, E15.4 )         M32.1449
 4142 CONTINUE                                                          M32.1450
 4145 IF ( T  .GT.  0.0 )   GO TO 5737                                  M38.1133
      IF ( ISTEAD  .NE.  0 )   GO TO 5737                               M23.1010
      IF ( INONL .LE. 0 )  GO TO 5737                                   M32.1452
      DO 5732  J=1, INONL                                               M23.1011
      IF ( NLTYPE(J)  .NE.  -96 )   GO TO 5732                          M23.1012
      KILL = 203                                                        M23.1013
      LSTAT(19) = 5732                                                  M23.1014
      RETURN                                                            M28.3910
 5732 CONTINUE                                                          M23.1016
 5737 IF ( NCOMP  .GT.  0 )   GO TO 419                                 M23.1017
      IF( INONL .GT. NUM99 )  NCOMP = 1                                 M42.3075
  419 IF ( FMAXFS  .EQ.  0.0 )   GO TO 447                              M20.1130
      TMAX = 0.0                                                        M20.1131
      N1 = 0                                                            M20.1132
      N2 = 0                                                            M20.1133
      IF ( KCONST  .EQ.  0 )   GO TO 433                                M20.1134
      DO 431  J=1, KCONST                                               M20.1135
      IF ( IFORM(J)  .NE.  14 )   GO TO 431                             M20.1136
      N1 = N1 + 1                                                       M20.1137
      IF ( TSTART(J)  .LT.  0.0 )   GO TO 424                           M20.1138
      N2 = N2 + 1                                                       M20.1139
  424 SFREQ(J) = FMINFS                                                 M20.1140
  431 CONTINUE                                                          M20.1141
      IF ( N1  .GT.  N2 )   GO TO 437                                   M20.1142
  433 KILL = 194                                                        M20.1143
      LSTAT(13) = N1                                                    M20.1144
      LSTAT(14) = N1 - N2                                               M20.1145
      LSTAT(19) = 433                                                   M20.1146
      RETURN                                                            M28.3911
  437 IF ( N2  .EQ.  0 )   GO TO 447                                    M20.1148
      WRITE (LUNIT6, 441)  KCONST, N1, N2                               M20.1149
  441 FORMAT ( /,  57H NOTICE  ----  THIS  'FREQUENCY SCAN'  DATA CASE CM20.1150
     1ONTAINS,  I5,  25H   EMTP SOURCES, OF WHICH,  I5,  6H   ARE    ,/,M20.1151
     2 15X,          102HSINUSOIDAL (TYPE-CODE 14).   BUT OF THESE SINUSM20.1152
     3OIDAL SOURCES, NOT ALL HAVE FIELD  'TSTART'  OF COLUMNS      ,/,  M20.1153
     4 15X,           98H61-70 PUNCHED NEGATIVE (WHICH WOULD INDICATE THM20.1154
     5AT SUCH A SOURCE IS PRESENT DURING THE STEADY-STATE          ,/,  M20.1155
     6 15X,           31HPHASOR SOLUTIONS).   THERE WERE,  I5, 60H   EXCM20.1156
     7EPTIONS OF THIS SORT.   THE USER IS REMINDED THAT ONLY       )    M20.1157
      WRITE (LUNIT6, 442)                                               M20.1158
  442 FORMAT (15X,    98HSINUSOIDAL SOURCES WHICH HAVE NEGATIVE  'TSTARTM20.1159
     1' WILL AFFECT THE EMTP SOLUTION.   BY DEFINITION OF          ,/,  M20.1160
     2 15X,           99H 'FREQUENCY SCAN' ,   THIS DATA CASE WILL INVOLM20.1161
     3VE ONLY STEADY-STATE PHASOR SOLUTIONS (THERE WILL BE         ,/,  M20.1162
     4 15X,           26HNO TRANSIENT SIMULATIONS).         )           M20.1163
  447 IF ( IBR  .GT.  0 )   GO TO 2436                                  M32.1453
C     DEGENERATE CASE WITHOUT LINEAR BRANCHES; ADD INFINITE R:          M32.1454
      WRITE (LUNIT6, 2430)                                              M32.1455
 2430 FORMAT (  /,  39H THIS IS A DEGENERATE DATA CASE WITHOUT,         M32.1456
     1              35H ANY LINEAR BRANCHES.  ADD INFINITE,             M32.1457
     2              34H RESISTANCE FROM NODE 2 TO GROUND. ,/ )          M32.1458
      IBR = 1                                                           M32.1459
      KBUS(1) = 2                                                       M32.1460
      MBUS(1) = 1                                                       M32.1461
      LENGTH(1) = 1                                                     M32.1462
      NR(1) = -1                                                        M32.1463
      IT = 1                                                            M32.1464
      TR(1) = 1.E10                                                     M32.1465
      TX(1) = 0.0                                                       M32.1466
      R(1) = 0.0                                                        M32.1467
      C(1) = 0.0                                                        M32.1468
 2436 IF (IPRSUP .LT. 3)   GO TO 1416                                   M32.1469
      WRITE (LUNIT6, 12416)                                             M32.1470
12416 FORMAT ( /,  21H SWITCH TABLE VECTORS  ,/,                        M20.1168
     1 56H     ROW   BUS1     BUS2    KPOS  KDEPSM  ISOURC  KSWTYP,     M33. 256
     2 9X, 6HTCLOSE, 9X, 6HADELAY, 10X, 5HTOPEN, 11X, 4HCRIT )          M32.1472
      IF ( KSWTCH  .LE.  0 )   GO TO 2428                               M32.1473
      DO 2406 I = 1, KSWTCH                                             M32.1474
      NDX2 = LSWTCH + I                                                 M32.1475
 2406 WRITE (LUNIT6, 2426) I, KMSWIT(I),KMSWIT(NDX2), KPOS(I),          M32.1476
     1 KDEPSW(I),ISOURC(I),KSWTYP(I),TCLOSE(I),ADELAY(I),               M32.1477
     2 TOPEN(I), CRIT(I)                                                M32.1478
 2426 FORMAT (7I8,  4E15.6 )                                            M32.1479
 2428 WRITE (LUNIT6, 13416)  (I, KBUS(I), MBUS(I), NR(I), LENGTH(I),    M32.1480
     1   KODEBR(I), KODSEM(I), I = 1, IBR)                              M15. 616
13416 FORMAT (/, 21H BRANCH TABLE VECTORS,/,  70H       ROW      KBUS   M15. 617
     1   MBUS        NR    LENGTH    KODEBR    KODSEM,/, ( 7I10) )      M31. 797
      WRITE (LUNIT6, 14416)  (I, IFORM(I), NODE(I), CREST(I), TIME1(I), M17. 394
     1   TSTART(I), SFREQ(I), I = 1, KCONST)                            M15. 620
14416 FORMAT (/, 21H SOURCE TABLE VECTORS,/, 30H       ROW     IFORM    M15. 621
     1  NODE,15X, 5HCREST, 15X, 5HTIME1, 14X, 6HTSTART,15X,5HSFREQ,/,   M15. 622
     2   (3I10, 4E20.10)   )                                            M15. 623
      WRITE (LUNIT6,2584) (I, TR(I), TX(I), R(I), C(I), I=1, IT)        M32.1481
 2584 FORMAT ( /, 40H ROWS 1 THROUGH IT OF PARAMETERS FOLLOW:           M32.1482
     1  ,/,    7X,  3HROW,  13X,  2HTR,  13X,  2HTX,  14X,              M32.1483
     2 1HR,  14X,  1HC  ,/,  ( I10,  4E15.5  ) )                        M32.1484
 1416 IF (IPRSUP .GE. 1)                                                M15. 624
     1 WRITE (LUNIT6, 1417)  KCONST, IBR, INONL, KSWTCH, ISTEAD,        M17. 395
     2 OMEGA, XOPT, COPT, TWOPI                                         M42.3076
 1417 FORMAT ( /,  22H NORMAL EXIT "OVER5A".,                           M32.1485
     1             40H  KCONST     IBR   INONL  KSWTCH  ISTEAD,         M32.1486
     2   8X, 5HOMEGA, 9X, 4HXOPT,                                       M32.1487
     3   9X, 4HCOPT, 8X, 5HTWOPI,  /, 22X, 5I8, 4E13.4  )               M32.1488
      LASTOV = NCHAIN                                                   M32.1489
      NCHAIN = NCHAIN + 1                                               M32.1490
      GO TO 9999                                                        M32.1491
 9000 LSTAT(16) = IPRINT                                                M42.3077
      KILL = 1                                                          M42.3078
 9999 RETURN                                                            M28.3914
      END                                                               M42.3079
      SUBROUTINE UMOFFS                                                 M27. 683
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     OVERLAY-5  U.M.  MODULE CALLED BY "OVER5A".                       M32.1492
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'UMDECK.FOR;/NOLIST'                                           
      IF ( NCLFIX  .GT.  0 )   GO TO 1758                               M27. 686
      NCLFIX = 20                                                       M27. 687
      NUMFIX = 3                                                        M27. 688
      IOTFIX = 50                                                       M27. 689
      IBSFIX = 60                                                       M27. 690
 1758 IF ( IPRSUP  .GE.  1 )                                            M27. 691
     1 WRITE (LUNIT6, 1853)   (NBYTE(I), I=1, 4),                       M27. 692
     2  NCLFIX, NUMFIX, IOTFIX, IBSFIX                                  M27. 693
 1853 FORMAT ( /,  18H BEGIN  "UMOFFS" .,                               M27. 694
     1  32H  NBYTE1  NBYTE2  NBYTE3  NBYTE4,                            M27. 695
     2  32H  NCLFIX  NUMFIX  IOTFIX  IBSFIX  ,/,                        M27. 696
     3  18X,  10I8  )                                                   M27. 697
      D5 = NBYTE(4)                                                     M27. 698
      D5 = D5 / NBYTE(3)                                                M27. 699
      IUREAC = 1                                                        M27. 700
C     STEP OVER ALL COIL-TABLE VECTORS NEXT.                            M27. 701
      IUGPAR = IUREAC + NCLFIX                                          M27. 702
      IUFPAR = IUGPAR + NCLFIX                                          M27. 703
      IUHIST = IUFPAR + NCLFIX                                          M27. 704
      IUUMRP = IUHIST + NCLFIX                                          M33. 257
      IUNOD1 = ( IUUMRP + NCLFIX ) / D5  +  1                           M33. 258
      IUNOD2 = IUNOD1 + NCLFIX                                          M32.1494
      IUJCLT = IUNOD2 + NCLFIX                                          M32.1495
      IUJCLO = IUJCLT + NCLFIX                                          M32.1496
      IUJTYP = IUJCLO + NCLFIX                                          M32.1497
C     STEP OVER ALL MACHINE-TABLE VECTORS NEXT.                         M27. 711
      IUNODO = IUJTYP + NUMFIX                                          M32.1498
      IUJTMT = IUNODO + NUMFIX                                          M32.1499
      IUHISM = ( IUJTMT + NUMFIX ) * D5  +  1                           M32.1500
      IUOMGM = IUHISM + NUMFIX                                          M27. 716
      IUOMLD = IUOMGM + NUMFIX                                          M27. 717
      IUTHAM = IUOMLD + NUMFIX                                          M27. 718
      IUREDU = IUTHAM + NUMFIX                                          M27. 719
      IUREDS = IUREDU + NUMFIX                                          M27. 720
      IUFLDS = IUREDS + NUMFIX                                          M27. 721
      IUFLDR = IUFLDS + NUMFIX                                          M27. 722
      IUREQU = IUFLDR + NUMFIX                                          M27. 723
      IUFLQS = IUREQU + NUMFIX                                          M27. 724
      IUFLQR = IUFLQS + NUMFIX                                          M27. 725
      IUJCDS = ( IUFLQR + NUMFIX ) / D5  +  1                           M32.1501
      IUJCQS = IUJCDS + NUMFIX                                          M32.1502
      IUFLXD = ( IUJCQS + NUMFIX ) * D5  +  1                           M32.1503
      IUFLXQ = IUFLXD + NUMFIX                                          M27. 729
      IUNPPA = ( IUFLXQ + NUMFIX ) / D5  +  1                           M32.1504
      IUROTM = ( IUNPPA + NUMFIX ) * D5  +  1                           M32.1505
      IUNCLD = ( IUROTM + NUMFIX ) / D5  +  1                           M32.1506
      IUNCLQ = IUNCLD + NUMFIX                                          M32.1507
      IUJTQO = IUNCLQ + NUMFIX                                          M32.1508
      IUJOMO = IUJTQO + NUMFIX                                          M32.1509
      IUJTHO = IUJOMO + NUMFIX                                          M32.1510
      IUREQS = ( IUJTHO + NUMFIX ) * D5  +  1                           M32.1511
      IUEPSO = IUREQS + NUMFIX                                          M27. 738
      IUDCOE = IUEPSO + NUMFIX                                          M27. 739
      IUKCOI = ( IUDCOE + NUMFIX ) / D5  +  1                           M32.1512
      IUVOLT = ( IUKCOI + NUMFIX ) * D5  +  1                           M32.1513
      IUANGL = IUVOLT + NUMFIX                                          M31. 799
      IUNODF = ( IUANGL + NUMFIX ) / D5  +  1                           M32.1514
      IUNODM = IUNODF + NUMFIX                                          M32.1515
      IUKUMO = IUNODM + NUMFIX                                          M32.1516
C     STEP OVER ALL OUTPUT-TABLE VECTORS NEXT.                          M27. 742
      IUJUMO = IUKUMO + IOTFIX                                          M32.1517
      IUUMOU = ( IUJUMO + IOTFIX ) * D5  +  1                           M32.1518
C     FINALLY, STEP OVER LAST VECTOR, AND CHECK FOR OVERFLOW.           M27. 746
      N5     = IUUMOU + IBSFIX                                          M32.1519
      LSTAT(45) = N5                                                    M27. 748
      IF ( N5  .LE.  LSPCUM )   GO TO 3458                              M27. 749
C     FOLLOWING IS TEMPORARY MESSAGE, TO BE MOVED TO ERROR OVERLAYS     M27. 750
C     LATER, WHEN WE HAVE TIME.                                         M27. 751
      WRITE (LUNIT6, 3451)  N5, LSPCUM, NCLFIX, NUMFIX, IOTFIX, IBSFIX  M28.3915
 3451 FORMAT ( /,  35H OVERFLOW ERROR STOP IN  "UMOFFS" . ,             M27. 753
     1             36H   INSUFFICIENT TOTAL WORKING SPACE.  ,/,         M27. 754
     2  1X,  10I8  )                                                    M27. 755
      CALL STOPTP                                                               
 3458 IF ( IPRSUP  .GE.  1 )                                            M27. 757
     1 WRITE (LUNIT6, 3465)  N5, LSPCUM                                 M27. 758
 3465 FORMAT ( /,  32H EXIT  "UMOFFS" .   N5, LSPCUM =,  2I6,/)         M36. 334
      IF ( NOUTPR  .EQ.  0 )                                            M27. 760
     1 WRITE (KUNIT6, 3472)  N5                                         M35. 894
 3472 FORMAT ( 40H+U.M. DATA BEGINS.  LIST-25 CELLS USED =, I5 )        M27. 762
      RETURN                                                            M27. 763
      END                                                               M27. 764
      SUBROUTINE UMDATA (                                               M28.3916
     1 REACL, GPAR, FPAR, HIST,UMCURP, NODVO1, NODVO2, JCLTAC,          M33. 259
     2 JCLOUT,JTYPE,NODOM, JTMTAC, HISTOM, OMEGM, OMOLD, THETAM,        M33. 260
     3 REAMDU, REAMDS, FLXDS, FLXDR, REAMQU, FLXQS, FLXQR,              M28.3919
     4 JCDSAT, JCQSAT, FLXD, FLXQ, NPPAIR, ROTMOM, NCLD,                M28.3920
     5 NCLQ, JTQOUT, JOMOUT, JTHOUT, REAMQS, EPSOM, DCOEF,              M28.3921
     6 KCOIL, VOLTUM, ANGLUM, NODFUM, NODMUM,                           M31. 804
     7 KUMOUT, JUMOUT, UMOUTP)                                          M31. 805
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION  REACL(1), GPAR(1), FPAR(1), HIST(1),UMCURP(1)          M33. 261
      DIMENSION  NODVO1(1), NODVO2(1), JCLTAC(1), JCLOUT(1)             M28.3924
      DIMENSION  JTYPE(1), NODOM(1), JTMTAC(1), HISTOM(1)               M28.3925
      DIMENSION  OMEGM(1), OMOLD(1), THETAM(1)                          M28.3926
      DIMENSION  REAMDU(1), REAMDS(1), FLXDS(1), FLXDR(1)               M28.3927
      DIMENSION  REAMQU(1), FLXQS(1), FLXQR(1)                          M28.3928
      DIMENSION  JCDSAT(1), JCQSAT(1), FLXD(1), FLXQ(1)                 M28.3929
      DIMENSION  NPPAIR(1), ROTMOM(1), NCLD(1)                          M28.3930
      DIMENSION  NCLQ(1), JTQOUT(1), JOMOUT(1), JTHOUT(1)               M28.3931
      DIMENSION  REAMQS(1), EPSOM(1), DCOEF(1), KCOIL(1)                M28.3932
      DIMENSION  VOLTUM(1), ANGLUM(1), NODFUM(1), NODMUM(1)             M31. 806
      DIMENSION  KUMOUT(1), JUMOUT(1), UMOUTP(1)                        M31. 807
       COMMON /UMLOCAL/ TEXTA, D1,D2,D3,D17, STAT59, FMUM, RMVAUM,
     1 RKVUM, S1UM, S2UM, ZLSBUM, S1QUM, S2QUM, AGLQUM, RAUM, XDUM,
     2 SQUM, XDPUM, XQPUM, XDPPUM, XQPPUM, TDPUM, TDPPUM, X0UM, RNUM,
     3 XNUM, XFUM, XDFUM, XDKDUM, XKDUM, XKQUM, XQKQUM, XGKQUM, XQUM,
     4 XQGUM, XGUM, DISTRF, HJUM, DSYNUM, DMUTUM, SPRING, DABSUM, 
     5 TQPPUM, AGLDUM, XLUM, N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,
     1 N14,N15,N16,N17,N18,N19,N20, JR,JF, NEXC, KCONEX, IBREXC,
     2 NSTAN, NUMASU, NMGEN, NMEXC, NTYPSM, NETRUN, NETRUM, NSMTPR,
     3 NSMTAC, NRSYN, NTORQ, MLUM, NPARUM, NGROUP, NALL, NANGRE,
     4 NEXCSW, LIMASU, LOPSS2, LOPSS1, LOPSS8, LOPSS9, LOPSS10,
     5 LOPSS4, NSHARE
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'UMDECK.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'SYNCOM.FOR;/NOLIST'                                           
      REAL*8        TEXTA(101), TEXT3,TEXT4,TEXT5                       M38.1134
      REAL*8        TEXTM, TESM1, TESM2, TESM3, TESM4, TESM5            M32.1522
      REAL*8        TESM6, TESM7, TESM8, TEXT6, TEXT7                   M38.1135
C. DATA INITIALIZATION:                                                 M28.3938
C     BE CAREFUL WITH FPAR(KCL) AND UMCURP(KCL). IN UMDATA AND          M36. 336
C      AND IN UMRENU THEY ARE USED DIFFERENTLY THAN IN SOLVUM.          M36. 337
C     FPAR(KCL) IN UMDATA AND UMRENU : ADRESS IBR OF LEAKAGE IND        M37.2462
C     FPAR(KCL+1) IN UMDATA AND UMRENU : ADRESS KCONST OF EXCIT         M37.2463
C       SOURCES TO REPRESENT IM EXCIT COILS (UMDATA CREATION)           M37.2464
C     UMCURP IN UMDATA AND UMRENU : USED TO STORE SHIFT IN POWER        M37.2465
C      NODES IF LOAD-FLOW IS REQUESTED. THEN AFTER LOAD-FLOW IS         M37.2466
C      IS COMPLETED, THESE SAME CELLS OF UMCURP ARE USED AS             M37.2467
C      POINTERS FOR THE SHIFT OF POWER RESISTANCES TO NETWORK.          M37.2468
      DATA TEXTA(1),TEXTA(2),TEXTA(3),TEXTA(4),TEXTA(5),                M29.1421
     1 TEXTA(6),TEXTA(7),TEXTA(8),TEXTA(9),TEXTA(10)                    M29.1422
     2 /6HUM-1  ,6HUM-2  ,6HUM-3  ,6HUM-4  ,6HUM-5  ,6HUM-6  ,          M29.1423
     3 6HUM-7  ,6HUM-8  ,6HUM-9  ,6HUM-10 /                             M29.1424
      DATA TEXTA(11),TEXTA(12),TEXTA(13),TEXTA(14),TEXTA(15),           M29.1425
     1 TEXTA(16),TEXTA(17),TEXTA(18),TEXTA(19),TEXTA(20)                M29.1426
     2 /6HUM-11 ,6HUM-12 ,6HUM-13 ,6HUM-14 ,6HUM-15 ,6HUM-16 ,          M29.1427
     3 6HUM-17 ,6HUM-18 ,6HUM-19 ,6HUM-20 /                             M29.1428
      DATA TEXTA(21),TEXTA(22),TEXTA(23),TEXTA(24),TEXTA(25),           M29.1429
     1 TEXTA(26),TEXTA(27),TEXTA(28),TEXTA(29),TEXTA(30)                M29.1430
     2 /6HTQGEN ,6HOMEGM ,6HTHETAM,6HIPA   ,6HIPB   ,6HIPC   ,          M29.1431
     3 6HIE1   ,6HIE2   ,6HIE3   ,6HIE4   /                             M29.1432
      DATA TEXTA(31),TEXTA(32),TEXTA(33),TEXTA(34),TEXTA(35),           M29.1433
     1 TEXTA(36),TEXTA(37),TEXTA(38),TEXTA(39),TEXTA(40)                M29.1434
     1 /6HIE5   ,6HIE6   ,6HIE7   ,6HIE8   ,6HIE9   ,6HIE10  ,          M29.1435
     1 6HIE11  ,6HIE12  ,6HIE13  ,6HIE14  /                             M29.1436
      DATA TEXTA(41), TEXTA(42), TEXTA(43), TEXTA(44),                  M32.1524
     1 TEXTA(45), TEXTA(46), TEXTA(47), TEXTA(48)                       M32.1525
     2 /6HUM1MXX, 6HUM1E1X, 6HUM1E2X, 6HUM1E3X,                         M32.1526
     3  6HUM2MXX, 6HUM2E1X, 6HUM2E2X, 6HUM2E3X/                         M32.1527
      DATA TEXTA(49), TEXTA(50), TEXTA(51), TEXTA(52),                  M32.1528
     1 TEXTA(53), TEXTA(54), TEXTA(55), TEXTA(56)                       M32.1529
     2 /6HUM3MXX, 6HUM3E1X, 6HUM3E2X, 6HUM3E3X,                         M32.1530
     3  6HUM4MXX, 6HUM4E1X, 6HUM4E2X, 6HUM4E3X/                         M32.1531
      DATA TEXTA(57), TEXTA(58), TEXTA(59), TEXTA(60)                   M32.1532
     1 /6HUM5MXX, 6HUM5E1X, 6HUM5E2X, 6HUM5E3X/                         M32.1533
      DATA TEXTA(61), TEXTA(62), TEXTA(63), TEXTA(64), TEXTA(65)        M32.1534
     1 /6HUM1NTR, 6HUM2NTR, 6HUM3NTR, 6HUM4NTR, 6HUM5NTR/               M32.1535
      DATA TEXTA(66), TEXTA(67), TEXTA(68), TEXTA(69), TEXTA(70)        M32.1536
     1 /6HUM1MS1, 6HUM2MS1, 6HUM3MS1, 6HUM4MS1, 6HUM5MS1/               M32.1537
      DATA TEXTA(71), TEXTA(72), TEXTA(73), TEXTA(74), TEXTA(75)        M32.1538
     1 /6HUM1MS2, 6HUM2MS2, 6HUM3MS2, 6HUM4MS2, 6HUM5MS2/               M32.1539
      DATA TEXTA(76), TEXTA(77), TEXTA(78), TEXTA(79), TEXTA(80)        M36. 342
     1 /6HUM1TLA, 6HUM1TLB, 6HUM1TLC, 6HUM2TLA, 6HUM2TLB/               M36. 343
      DATA TEXTA(81), TEXTA(82), TEXTA(83), TEXTA(84), TEXTA(85)        M36. 344
     1 /6HUM2TLC, 6HUM3TLA, 6HUM3TLB, 6HUM3TLC, 6HUM4TLA/               M36. 345
      DATA TEXTA(86), TEXTA(87), TEXTA(88), TEXTA(89), TEXTA(90)        M36. 346
     1 /6HUM4TLB, 6HUM4TLC, 6HUM5TLA, 6HUM5TLB, 6HUM5TLC/               M36. 347
      DATA TEXTA(91), TEXTA(92), TEXTA(93), TEXTA(94)                   M38.1136
     1 /6HFLUXMD, 6HIMD   , 6HFLUXMQ, 6HIMQ   /                         M38.1137
      DATA TEXTA(95), TEXTA(96), TEXTA(97), TEXTA(98)                   M38.1138
     1 /6HIP0   , 6HIPD   , 6HIPQ   , 6HTQEXC /                         M38.1139
      DATA TEXTA(99),TEXTA(100),TEXTA(101)                              M38.1140
     1 /6HUM1MCC, 6HUM2MCC, 6HUM3MCC/                                   M38.1141
      DATA TEXTM / 6HMORE  /                                            M32.1540
      DATA TESM1,TESM2,TESM3,TESM4,TESM5,TESM6,TESM7,TESM8,TESM9        M38.1142
     1 /6HSMDATA,6HTOLERA,6HPARAME,6HALL   ,6HNONE  ,6HEXTEND,          M32.1542
     2  6HFINISH,6H PART ,6HSHARE /                                     M38.1143
C   IF MECH NETWORK OPTION IS USED, ROTMOM IS USED TO STORE             M37.2469
C      FREQUENCY OF NETWORK CONNECTED TO POWER COIL. INITIALI -         M37.2470
C      ZATION IS IN UMRENU, EXCEPT IF SM TYPE-59 IS USED.               M37.2471
      IF ( IPRSUP  .GE.  1 )                                            M32.1544
     1 WRITE ( LUNIT6, 4567 )                                           M32.1545
 4567 FORMAT ( 24H  "BEGIN MODULE UMDATA." )                            M32.1546
      IF (NUMUM .EQ. 0) GO TO 40                                        M37.2472
      WRITE (LUNIT6,30)                                                 M37.2473
   30 FORMAT(/,45H UM ERROR STOP. YOU HAVE REQUESTED ANOTHER UM,        M37.2474
     1         44H TO BE INCLUDED TO THIS DATA CASE. THE RULES,         M37.2475
     2         29H REGARDING THE ORDERING OF UM)                        M37.2476
      WRITE (LUNIT6,31)                                                 M37.2477
   31 FORMAT(  39H DATA CARDS HAVE HOWEVER BEEN VIOLATED.,              M37.2478
     1         43H THE DATA CARDS OF THIS UM SHOULD BE PLACED,          M37.2479
     2         32H IMMEDIATELY BEHIND THOSE OF THE                      M37.2480
     3  ,/,    35H PREVIOUS UM IN ACCORDANCE WITH THE,                  M37.2481
     4         18H FOLLOWING RULES :)                                   M37.2482
      WRITE (LUNIT6,32)                                                 M37.2483
   32 FORMAT(/,43H (1). THE DATA CARDS OF THE NEXT UM HAVE TO,          M37.2484
     1         44H FOLLOW IMMEDIATELY THOSE OF THE PREVIOUS UM,         M37.2485
     2         25H WITHOUT THE INSERTION OF)                            M37.2486
      WRITE(LUNIT6,33)                                                  M37.2487
   33 FORMAT(  34H A BLANK CARD AND WITHOUT REPEATED,                   M37.2488
     1         40H INSERTION OF THE CLASS-1 UM DATA CARDS.,             M37.2489
     2         40H  THESE CLASS-1 CARDS, CONSISTING OF TWO )            M37.2490
      WRITE(LUNIT6,34)                                                  M37.2491
   34 FORMAT(  36H CARDS AND A BLANK TERMINATION CARD,,                 M37.2492
     1         43H CONTAINS INITIAL INFORMATION REGARDING THE,          M37.2493
     2         41H READING OF ALL UM CARDS TO FOLLOW. IT IS)            M37.2494
      WRITE(LUNIT6,35)                                                  M37.2495
   35 FORMAT(  43H THEREFORE TO BE PLACED ON TOP AND NEVER TO,          M37.2496
     1         37H BE REPEATED BETWEEN THE CARDS OF THE,                M37.2497
     2         16H DIFFERENT UM'S.)                                     M37.2498
      WRITE(LUNIT6,36)                                                  M37.2499
   36 FORMAT(/,41H (2). THE DATA CARDS OF THE LAST UM TO BE,            M37.2500
     1         36H INCLUDED TO THIS DATA CASE IS TO BE,                 M37.2501
     2         41H TERMINATED WITH A BLANK CARD, INDICATING             M37.2502
     3     ,/, 33H THE ENDING OF ALL UM DATA CARDS.)                    M37.2503
      GO TO 9600                                                                
C. BUSUM ADJUSTMENT DUE TO VARIABLE NUMFIX :                            M28.3955
   40 DO 42 N1 = 1,NUMFIX                                                       
   42 BUSUM(N1) = TEXTA(N1)                                                     
      DO 50 N1=1,28                                                             
      N2 = N1 + NUMFIX                                                  M28.3957
      N3 = N1 + 12                                                              
      IF (N1 .GT. 11) GO TO 50                                          M38.1148
      N3 = N1 + 87                                                              
      IF (N1 .GT. 3) GO TO 50                                           M38.1150
      N3 = N1 + 20                                                              
   50 BUSUM(N2) = TEXTA(N3)                                             M38.1152
      PI = TWOPI * ONEHAF                                               M29.1441
      D2 = 2.0                                                          M28.3961
      D3 = 3.0                                                          M28.3962
      SROOT2 = SQRTZ(D2)                                                M28.3963
      SROOT3 = SQRTZ(D3)                                                M28.3964
      OMEGRF = TWOPI * STATFR                                           M28.3965
C  START READING UM DATA ***************************************        M32.1547
C    NOTE : THE FOLLOWING VARIABLE NAMES ARE RESERVED FOR               M32.1548
C           LOOPING DURING READING OF THE DATA INPUT :                  M32.1549
      NUMUM = 0                                                         M37.2506
      NCLTOT = 0                                                        M32.1551
      NSMTAC = 0                                                        M32.1552
      NSMACH = 0                                                        M36. 348
C   NSMTAC IS THE TOTAL NUMBER OF ADDITIONAL ENTRIES TO                 M32.1553
C    UMOUTP(NUMOUT) BECAUSE OF SM TACS TRANSFER.                        M32.1554
      N1 = 0                                                            M32.1555
C.USER1 : GENERAL SPECIFICATIONS (CLASS 1 UM DATA CARDS)                M36. 349
C     READ INPUT CARD USING CIMAGE                                      M28.3967
      CALL CIMAGE                                                       M28.3968
      DECODE (80,60, ABUFF(1)) INPU,INITUM,BUS3,LIMASU,LOOPSS(8)        M39.1034
   60 FORMAT(2I1,A6,I6,I1)                                              M39.1035
      IF (BUS3 .EQ. TESM1) INITUM = 1                                   M39.1036
      IF (LIMASU .EQ. 0) LIMASU = 10                                    M32.1561
      IF (LOOPSS(8) .EQ. 0) GO TO 105                                   M33. 273
      IF (NOUTPR .EQ. 0) WRITE (KUNIT6, 102)                            M35. 895
  102 FORMAT(40H+UM POWER COIL INTERFACING BY PREDICTION)               M39.1037
      GO TO 150                                                         M36. 352
  105 IF ( NOUTPR  .EQ.  0 ) WRITE (KUNIT6, 110)                        M35. 896
  110 FORMAT (21H+UM FULLY COMPENSATED)                                 M33. 278
C     READ(LUNIT5,203) JTYPE(N1),NCLD(N1),NCLQ(N1),JTQOUT(N1),          M28.3978
C    VJOMOUT(N1),JTHOUT(N1),BUM3(N1),BUM4(N1),NPPAIR(N1),               M28.3979
C    VROTMOM(N1),DCOEF(N1),EPSOM(N1),OMEGM(N1),REAMDU(N1),              M28.3980
C    VJCDSAT(N1),REAMDS(N1),FLXDS(N1),FLXDR(N1),THETAM(N1),             M28.3981
C    VREAMQU(N1),JCQSAT(N1),REAMQS(N1),FLXQS(N1),FLXQR(N1),NOREAD       M28.3982
  203 FORMAT(3I2,3I1,2A6,I2,3E14.5/2E14.5,I1,3E14.5/2E14.5,I1,          M28.3983
     V3E14.5,I1)                                                        M28.3984
C  DECISION ON M31 OR M32 DATA INPUT ***************************        M32.1567
C    WITH M32 , THE INPUT IS MODULAR.                                   M32.1568
  150 CALL CIMAGE                                                       M32.1569
      DECODE (80,1207,ABUFF(1)) TEXT3, TEXT4, TEXT5                     M32.1570
      IF (TEXT3 .NE. BLANK) GO TO 3202                                  M32.1571
      IF (TEXT4 .NE. BLANK) GO TO 3202                                  M32.1572
      IF (TEXT5 .NE. BLANK) GO TO 3202                                  M32.1573
      IF (NOUTPR .EQ. 0) WRITE (KUNIT6, 1001)                           M35. 897
 1001 FORMAT ( 40H+BLANK CARD ENDING CLASS-1 UM DATA CARDS  )           M33. 280
      GO TO 11202                                                       M32.1576
C  DATA INPUT FOR M31 AND OLDER VERSIONS ***********************        M32.1577
C. USER2 : MACH-TABLE INPUT FOR M31 AND OLDER                           M32.1578
C     READ INPUT CARD USING CIMAGE                                      M28.3985
 1202 CALL CIMAGE                                                       M28.3986
 1203 N6 = 0                                                            M32.1579
      DECODE (80, 1207, ABUFF(1))  TEXT3, TEXT4, TEXT5                  M32.1580
 1207 FORMAT ( 13A6, A2 )                                               M31. 811
      IF ( TEXT3  .NE.  BLANK )   GO TO 3202                            M31. 812
      IF ( TEXT4  .NE.  BLANK )   GO TO 3202                            M31. 813
      IF ( TEXT5  .NE.  BLANK )   GO TO 3202                            M31. 814
      IF ( NOUTPR  .EQ.  0 )                                            M28.3989
     1 WRITE (KUNIT6, 2202)                                             M35. 898
 2202 FORMAT (  33H+BLANK CARD ENDING MACHINE TABLE.  )                 M30. 139
      GO TO 300                                                         M38.1153
 3202 NUMUM = NUMUM + 1                                                 M28.3993
      IF (IPRSUP .GE. 1) WRITE(LUNIT6,170) NUMUM                        M36. 353
  170 FORMAT(37H0THE MACHINE-TABLE INPUT OF UM NUMBER,I3,1H:)           M36. 354
      IF ( NUMUM  .LE.  NUMFIX )   GO TO 5202                           M28.3994
      WRITE (LUNIT6, 4202)  NUMUM                                       M28.3995
 4202 FORMAT ( /,  66H OVERFLOW U.M. MACHINE TABLE IN MODULE "UMDATA".  M39.1038
     1CURRENT CASE HAS, 2X, I4, 2X, 52HU.M.,  NUMFIX ON THE CARD FOR ABSM39.1039
     2OLUTE UM DIMENSIONS, /,28 H MUST BE SET TO NO LESS THAN, 2X, I4,  M39.1040
     3  1H.  )                                                          M39.1041
      GO TO 9600                                                                
 5202 N1 = NUMUM                                                        M28.3999
      DECODE (80, 1204, ABUFF(1))  JTYPE(N1), NCLD(N1),                 M32.1583
     1  NCLQ(N1),JTQOUT(N1), JOMOUT(N1), JTHOUT(N1), BUS3, BUS4,        M28.4001
     2  NPPAIR(N1), ROTMOM(N1), DCOEF(N1), EPSOM(N1)                    M28.4002
 1204 FORMAT ( 3I2, 3I1, 2A6, I2, 3E14.5 )                              M32.1584
      IF ( NOUTPR  .EQ.  0 )                                            M28.4004
     1 WRITE (KUNIT6, 3203)  JTYPE(N1), NCLD(N1), NCLQ(N1)              M35. 899
 3203 FORMAT (  21H+MACH-TABLE CARD   1.,  3I5  )                       M32.1585
      DO 204 N10=1,NTOT                                                 M28.4007
      IF (BUS3 .EQ. BLANK ) GO TO 205                                   M37.2508
      IF (BUS3 .NE. BUS(N10)) GO TO 204                                 M28.4008
      NODOM(N1) = N10                                                   M28.4009
      ROTMOM(N1) = TWOPI * STATFR                                       M37.2509
      GO TO 205                                                         M28.4010
  204 CONTINUE                                                          M28.4011
      WRITE (LUNIT6, 3204)  BUS3                                        M28.4012
 3204 FORMAT ( /,   36H ERROR STOP.   THE NODE NAME  "BUS3",            M30. 142
     1              34H ON THE LAST-READ CARD IS NOT THAT               M30. 143
     2  ,/,         30H OF ANY ELECTRIC NETWORK NODE.,                  M30. 144
     3               6H BUS3=,  A6  )                                   M30. 145
      GO TO 9600                                                                
  205 JTMTAC(N1) = 0                                                    M28.4018
      IF (BUS3 .NE. BLANK ) GO TO 213                                   M37.2511
      DO 207 J = 1,KTAB                                                 M28.4019
      NDX1 = ILNTAB( KLNTAB + J )                                       M38.1154
      IF (BUS4 .EQ. TEXVEC(NDX1)) GO TO 209                             M38.1155
  207 CONTINUE                                                          M28.4022
      IF ( BUS4  .EQ.  BLANK )   GO TO 213                              M28.4023
      WRITE (LUNIT6, 3207)  BUS4                                        M28.4024
 3207 FORMAT ( /,   36H ERROR STOP.   THE TACS NAME  "BUS4",            M30. 146
     1              25H ON THE LAST-READ CARD IS,                       M30. 147
     2              22H UNRECOGNIZED.   BUS4=,  A6  )                   M30. 148
      GO TO 9600                                                                
  209 NDX1 = KXTCS + J                                                  M28.4029
      JTMTAC(N1) = NDX1                                                 M28.4030
C     READ INPUT CARD USING CIMAGE                                      M28.4031
  213 CALL CIMAGE                                                       M28.4032
      DECODE (80, 4203, ABUFF(1))  OMEGM(N1), REAMDU(N1),               M28.4033
     1  JCDSAT(N1), REAMDS(N1), FLXDS(N1), FLXDR(N1)                    M28.4034
 4203 FORMAT ( 2E14.5,  I1,  3E14.5 )                                   M28.4035
      IF ( NOUTPR  .EQ.  0 )                                            M28.4036
     1 WRITE (KUNIT6, 5203)  OMEGM(N1), REAMDU(N1)                      M35. 900
 5203 FORMAT (  21H+MACH-TABLE CARD   2., 2E12.4)                       M32.1587
C     READ INPUT CARD USING CIMAGE                                      M28.4039
      CALL CIMAGE                                                       M28.4040
      DECODE (80, 4203, ABUFF(1))  THETAM(N1), REAMQU(N1),              M28.4041
     1  JCQSAT(N1), REAMQS(N1), FLXQS(N1), FLXQR(N1)                    M28.4042
      IF (REAMQU(N1) .GT. 0.0) GO TO 6199                               M36. 355
      WRITE(LUNIT6,6198) N1                                             M36. 356
 6198 FORMAT( /, 41H ERROR STOP. INCORRECT UM DATA INPUT. THE,          M36. 357
     1           42H RESULT IS THAT THE Q-AXIS MAIN INDUCTANCE          M36. 358
     2  ,/,7X,   39H IS EITHER ZERO OR NEGATIVE FOR MACHINE,            M36. 359
     3            7H NUMBER, I4, 1H.)                                   M36. 360
      GO TO 9600                                                                
 6199 IF ( NOUTPR  .EQ.  0 )                                            M36. 362
     1 WRITE (KUNIT6, 6203)  THETAM(N1), REAMQU(N1)                     M35. 901
 6203 FORMAT (  21H+MACH-TABLE CARD   3., 2E12.4)                       M32.1589
      IF ( IPRSUP  .GE.  1 )                                            M28.4046
     1 WRITE(LUNIT6,214) JTYPE(N1),NCLD(N1),NCLQ(N1),JTQOUT(N1),        M28.4047
     VJOMOUT(N1),JTHOUT(N1), BUS3, BUS4, NPPAIR(N1),                    M28.4048
     VROTMOM(N1),DCOEF(N1),EPSOM(N1),OMEGM(N1),REAMDU(N1),              M28.4049
     VJCDSAT(N1),REAMDS(N1),FLXDS(N1),FLXDR(N1),THETAM(N1),             M28.4050
     VREAMQU(N1),JCQSAT(N1),REAMQS(N1),FLXQS(N1),FLXQR(N1)              M28.4051
  214 FORMAT(1H0,3I2,3I1,2A6,I2,3E14.5/1H ,2E14.5,I1,3E14.5/            M28.4052
     V1H ,2E14.5,I1,3E14.5)                                             M28.4053
      IF (INITUM .EQ. 0) GO TO 249                                      M32.1590
      CALL CIMAGE                                                       M31. 816
      D1 = 1.0E+3                                                       M32.1591
      N5 = 4                                                            M32.1592
      N6 = 0                                                            M32.1593
      DECODE (80,6204,ABUFF(1)) VOLTUM(N1), ANGLUM(N1),BUS5,            M32.1594
     1  BUS6,DISTRF                                                     M32.1595
 6204 FORMAT(2E14.5, 2A6, E14.5)                                        M32.1596
      IF (NOUTPR .EQ. 0)                                                M31. 819
     1 WRITE (KUNIT6, 6205)  VOLTUM(N1), ANGLUM(N1)                     M35. 902
 6205 FORMAT(   21H+MACH-TABLE CARD   4., 2E12.4)                       M32.1597
      IF (JTYPE(N1) .EQ. 3) GO TO 230                                   M31. 822
      IF (JTYPE(N1) .EQ. 4) GO TO 230                                   M31. 823
      IF (JTYPE(N1) .EQ. 5) GO TO 230                                   M31. 824
      DO 220 N10 = 1,NTOT                                               M31. 825
      IF (BUS5 .NE. BUS(N10)) GO TO 220                                 M31. 826
      NODFUM(N1) = N10                                                  M31. 827
      DO 219 N11 = 1,KCONST                                             M31. 828
      N12 = NODE(N11)                                                   M31. 829
      IF (N12 .LT. 0) N12 = - N12                                       M31. 830
      IF (NODFUM(N1) .NE. N12) GO TO 219                                M31. 831
      NODFUM(N1) = N11                                                  M31. 832
      GO TO 230                                                         M31. 833
  219 CONTINUE                                                          M31. 834
  220 CONTINUE                                                          M31. 835
      WRITE (LUNIT6, 222)  BUS5                                         M31. 836
  222 FORMAT ( /,   36H ERROR STOP.   THE NODE NAME  "BUSF",            M31. 837
     1              34H ON THE LAST-READ CARD IS NOT THAT               M31. 838
     2  ,/,         37H OF ANY ELECTRIC NETWORK SOURCE NODE.,           M31. 839
     3               6H BUSF=,  A6  )                                   M31. 840
      GO TO 9600                                                                
  230 DO 233 N10 = 1,NTOT                                               M32.1598
      IF (BUS6 .NE. BUS(N10)) GO TO 233                                 M32.1599
      DO 232 N11 = 1,KCONST                                             M32.1600
      N12 = NODE(N11)                                                   M31. 846
      IF (N12 .LT. 0) N12 = - N12                                       M31. 847
      IF (N10 .NE. N12) GO TO 232                                       M32.1601
      CREST(N11) = D1 * DISTRF                                          M32.1602
      IF (N6 .NE. 0) GO TO 231                                          M32.1603
      NODMUM(N1) = N11                                                  M32.1604
      IF (DISTRF .EQ. 0.0) CREST(N11) = D1                              M32.1605
  231 TSTART(N11) = - 7777.                                             M32.1606
      GO TO 240                                                         M31. 850
  232 CONTINUE                                                          M32.1607
  233 CONTINUE                                                          M32.1608
      WRITE (LUNIT6, 234) BUS6                                          M31. 853
  234 FORMAT ( /,   36H ERROR STOP.   THE NODE NAME  "BUSM",            M31. 854
     1              34H ON THE LAST-READ CARD IS NOT THAT               M31. 855
     2  ,/,         37H OF ANY ELECTRIC NETWORK SOURCE NODE.,           M31. 856
     3               6H BUSM=,  A6  )                                   M31. 857
      GO TO 9600                                                                
  240 CALL CIMAGE                                                       M32.1609
      DECODE (80,6240,ABUFF(1)) TEXT3                                   M32.1610
 6240 FORMAT(A6)                                                        M32.1611
      IF (TEXT3 .NE. TEXTM) GO TO 248                                   M32.1612
      N5 = N5 + 1                                                       M32.1613
      N6 = 1                                                            M32.1614
      DECODE (80,6241,ABUFF(1)) TEXT3, BUS6, DISTRF                     M32.1615
 6241 FORMAT(A6, 28X, A6, E14.5)                                        M32.1616
      IF (NOUTPR .EQ. 0)                                                M32.1617
     1 WRITE (KUNIT6, 6242)  N5, DISTRF                                 M35. 903
 6242 FORMAT( 16H+MACH-TABLE CARD, I4, 1H., E12.4)                      M32.1619
      GO TO 230                                                         M32.1620
  248 GO TO 1203                                                        M32.1621
  249 GO TO 1202                                                        M32.1622
C. IF FLUX OUTPUT IS DESIRED :                                          M28.4055
C. USER3 : COIL-TABLE INPUT FOR M31 AND OLDER                           M32.1623
  300 IF ( IPRSUP  .GE.  1 )                                            M28.4059
     1 WRITE(LUNIT6,301)                                                M28.4060
  301 FORMAT(22H0THE COIL-TABLE INPUT:  ,/,  1X  )                      M28.4061
C. AVOID CONFLICT BLANK TERMINATION CARD WITH DUMMY COIL                M28.4063
C. AND ADJUSTMENT OF NCLD AND NCLQ FOR UM TYPE 4 :                      M28.4064
      N5 = 0                                                            M28.4065
      DO 302 N10 = 1,NUMUM                                              M28.4066
      IF (JTYPE(N10) .NE. 4) GO TO 302                                  M28.4067
      N5 = N5 + 1                                                       M28.4068
      NCLD(N10) = 1                                                     M28.4069
      NCLQ(N10) = 1                                                     M28.4070
  302 N5 = N5 + NCLD(N10) + NCLQ(N10)                                   M28.4071
      N5 = N5 + 3 * NUMUM                                               M28.4072
C     READ(LUNIT5,303) GPAR(N1),REACL(N1),BUM1(N1),BUM2(N1),            M28.4073
C    VBUM6(N1),JCLOUT(N1),HIST(N1),NOREAD                               M28.4074
  303 FORMAT(2E14.5,3A6,I1,E14.5 )                                      M28.4075
C     READ INPUT CARD USING CIMAGE                                      M28.4076
 3302 CALL CIMAGE                                                       M28.4077
      DECODE (80, 1207, ABUFF(1))  TEXT3, TEXT4, TEXT5                  M31. 860
      IF ( TEXT3  .NE.  BLANK )   GO TO 4303                            M31. 861
      IF ( TEXT4  .NE.  BLANK )   GO TO 4303                            M31. 862
      IF ( TEXT5  .NE.  BLANK )   GO TO 4303                            M31. 863
      IF ( NCLTOT .LT. N5 ) GO TO 4303                                  M28.4080
      IF ( NOUTPR  .EQ.  0 )                                            M28.4081
     1 WRITE (KUNIT6, 4302)                                             M35. 904
 4302 FORMAT (   30H+BLANK CARD ENDING COIL TABLE.  )                   M30. 151
      GO TO 500                                                         M28.4084
 4303 NCLTOT = NCLTOT + 1                                               M28.4085
      FPAR(NCLTOT) = 0.0                                                M38.1156
      IF ( NCLTOT  .LE.  NCLFIX )   GO TO 6302                          M28.4086
      WRITE (LUNIT6, 5302)  NCLTOT                                      M28.4087
 5302 FORMAT ( /,   26H OVERFLOW U.M. COIL TABLE.,                      M30. 152
     1              11H   NCLTOT =,  I5  )                              M30. 153
      GO TO 9600                                                                
 6302 N1 = NCLTOT                                                       M28.4091
      DECODE (80, 303, ABUFF(1))  GPAR(N1), REACL(N1), BUS1,            M28.4092
     1  BUS2, BUS6, JCLOUT(N1), HIST(N1)                                M28.4093
      IF ( NOUTPR  .EQ.  0 )                                            M28.4094
     1 WRITE (KUNIT6, 2203)                                             M35. 905
     2            NCLTOT,   GPAR(N1), REACL(N1), JCLOUT(N1)             M28.4096
 2203 FORMAT (  5H+COIL,  I4,  1H.,  2E14.5,  I5  )                     M30. 154
      N11 = 0                                                           M28.4098
      N12 = 0                                                           M28.4099
      DO 306 N10 = 1,NTOT                                               M28.4100
      IF (N11 .NE. 0) GO TO 304                                         M28.4101
      IF (BUS1 .NE. BUS(N10)) GO TO 304                                 M28.4102
      NODVO1(N1) = N10                                                  M28.4103
      N11 = 1                                                           M28.4104
      IF (N12 .NE. 0) GO TO 305                                         M28.4105
  304 IF (BUS2 .NE. BUS(N10)) GO TO 305                                 M28.4106
      NODVO2(N1) = N10                                                  M28.4107
      N12 = 1                                                           M28.4108
  305 N13 = N11 * N12                                                   M28.4109
      IF (N13 .NE. 0) GO TO 307                                         M28.4110
  306 CONTINUE                                                          M28.4111
      IF ( N11  .EQ.  0 )                                               M28.4112
     1 WRITE (LUNIT6, 3206)  BUS1                                       M28.4113
 3206 FORMAT ( /,    33H ERROR STOP.  JUST-READ COIL CARD,              M30. 155
     1               25H BEARS ILLEGAL NODE NAME:,  A6  )               M30. 156
      IF ( N12  .EQ.  0 )                                               M28.4116
     1 WRITE (LUNIT6, 3206)  BUS2                                       M28.4117
      GO TO 9600                                                                
  307 JCLTAC(N1) = 0                                                    M39.1042
      IF (BUS6 .EQ. BLANK) GO TO 313                                    M39.1043
      DO 309 J=1,KTAB                                                   M28.4120
      NDX1 = ILNTAB( KLNTAB + J )                                       M38.1157
      IF (BUS6 .EQ. TEXVEC(NDX1)) GO TO 311                             M38.1158
  309 CONTINUE                                                          M28.4123
      WRITE (LUNIT6, 3207)  BUS6                                        M28.4125
      GO TO 9600                                                                
  311 NDX1 = KXTCS + J                                                  M28.4127
      JCLTAC(N1) = NDX1                                                 M39.1044
  313 IF (IPRSUP .GE. 1)                                                M28.4129
     1 WRITE(LUNIT6,314) GPAR(N1),REACL(N1), BUS1, BUS2,                M28.4130
     1 BUS6, JCLOUT(N1),HIST(N1)                                        M28.4131
  314 FORMAT(1H ,2E14.5,3A6,I1,E14.5 )                                  M28.4132
      GO TO 3302                                                        M28.4133
C  DATA INPUT FOR M32 AND NEWER VERSIONS ********************           M32.1624
C. USER2 : MACH-TABLE INPUT FOR M32 AND NEWER                           M32.1625
C     CLASS 2 AND CLASS 3 UM DATA CARDS                                 M36. 363
11202 CALL CIMAGE                                                       M32.1627
      DECODE (80,11207, ABUFF(1))  TEXT3, TEXT4, TEXT5                  M32.1628
11207 FORMAT ( 13A6, A2 )                                               M32.1629
      IF ( TEXT3  .NE.  BLANK )   GO TO 13202                           M32.1630
      IF ( TEXT4  .NE.  BLANK )   GO TO 13202                           M32.1631
      IF ( TEXT5  .NE.  BLANK )   GO TO 13202                           M32.1632
      IF ( NOUTPR  .EQ.  0 )                                            M32.1633
     1 WRITE (KUNIT6, 12202)                                            M35. 906
12202 FORMAT (  27H+BLANK CARD ENDING UM DATA.)                         M32.1635
      GO TO 500                                                         M32.1636
13202 NUMUM = NUMUM + 1                                                 M32.1637
      IF ( NUMUM  .LE.  NUMFIX )   GO TO 17000                          M32.1638
      WRITE (LUNIT6, 14202)  NUMUM                                      M32.1639
14202 FORMAT ( /,  37HOVERFLOW U.M. MACHINE TABLE.  NUMUM =,            M32.1640
     1  I4, 36HINCREASE NUMFIX IN CARD FOR ABSOLUTE                     M32.1641
     2  ,/, 15H UM DIMENSIONS.)                                         M32.1642
      GO TO 9600                                                                
C  CHECKING OF DATA INPUT FORMAT ******************************         M36. 364
17000 N10 = 0                                                                   
      DECODE(80, 17002, ABUFF(1)) N10                                   M32.1646
17002 FORMAT(I2)                                                        M36. 365
      IF (N10 .NE. 19) GO TO 17004                                      M37.2520
      WRITE (LUNIT6,30)                                                 M37.2521
      WRITE (LUNIT6,31)                                                 M37.2522
      WRITE (LUNIT6,32)                                                 M37.2523
      WRITE (LUNIT6,33)                                                 M37.2524
      WRITE (LUNIT6,34)                                                 M37.2525
      WRITE (LUNIT6,35)                                                 M37.2526
      WRITE (LUNIT6,36)                                                 M37.2527
      GO TO 9600                                                                
17004 IF (N10 .GE. 50 .AND. N10 .LT. 60) GO TO 17040                    M37.2529
      IF (N10 .LT. 50) GO TO 15202                                      M32.1648
C  START READING SM TYPE -50 TO 59 DATA INPUT FORMAT **********         M36. 367
      JF = 0                                                            M42.3093
      JR = 0                                                            M42.3094
17040 CALL UMDATB (REACL,GPAR,FPAR,NODVO1,NODVO2,JCLTAC,                M42.3095
     1 JTYPE,NODOM,NCLD,JTMTAC,REAMDU,REAMDS,FLXDS,FLXDR,REAMQU,        M42.3096
     2 FLXQS,FLXQR,JCDSAT,JCQSAT,NPPAIR,ROTMOM,NCLQ,JTQOUT,             M42.3097
     3 JTHOUT,REAMQS,VOLTUM,ANGLUM,NODFUM,NODMUM,KUMOUT,JUMOUT,         M42.3098
     4 JCLOUT, DCOEF, JOMOUT, UMOUTP )                                  M42.3099
      IF (JF .EQ. 1) GOTO 11202                                         M42.3100
      IF (JR .EQ. 1) RETURN                                                     
C  END READING SM TYPE-59 DATA INPUT $$$$$$$$$$$$$$$$$$$$$$$$$$         M32.2562
C    START READING PURE UM DATA INPUT FORMAT ******************         M36. 606
15202 N1 = NUMUM                                                        M32.2563
      DECODE (80, 11203, ABUFF(1))  JTYPE(N1), NCLD(N1),                M32.2564
     1  NCLQ(N1),JTQOUT(N1), JOMOUT(N1), JTHOUT(N1), BUS3, BUS4,        M32.2565
     2  NPPAIR(N1), ROTMOM(N1), DCOEF(N1), EPSOM(N1), D10               M38.1316
11203 FORMAT ( 3I2, 3I1, 2A6, I2, 4E14.5 )                              M38.1317
      IF ( NOUTPR  .EQ.  0 )                                            M32.2568
     1 WRITE (KUNIT6, 13203)  NUMUM, JTYPE(N1)                          M35. 956
13203 FORMAT(5H+UM -, I3, 26H   MACH CARD  1.   TYPE = ,I2)             M32.2570
      DO 10204 N10=1,NTOT                                               M32.2571
      IF (BUS3 .EQ. BLANK ) GO TO 10205                                 M37.2565
      IF (BUS3 .NE. BUS(N10)) GO TO 10204                               M32.2572
      NODOM(N1) = N10                                                   M32.2573
      IF (D10 .EQ. 0.0) D10 = STATFR                                    M38.1318
      ROTMOM(N1) = TWOPI * D10                                          M38.1319
      GO TO 10205                                                       M32.2574
10204 CONTINUE                                                          M32.2575
      WRITE (LUNIT6, 13204)  BUS3                                       M32.2576
13204 FORMAT ( /,   36H ERROR STOP.   THE NODE NAME  "BUS3",            M32.2577
     1              34H ON THE LAST-READ CARD IS NOT THAT               M32.2578
     2  ,/,         30H OF ANY ELECTRIC NETWORK NODE.,                  M32.2579
     3               6H BUS3=,  A6  )                                   M32.2580
      GO TO 9600                                                                
10205 JTMTAC(N1) = 0                                                    M32.2582
      IF (BUS3 .NE. BLANK ) GO TO 10213                                 M37.2568
      DO 10207 J = 1,KTAB                                               M32.2583
      NDX1 = ILNTAB( KLNTAB + J )                                       M38.1320
      IF (BUS4 .EQ. TEXVEC(NDX1)) GO TO 10209                           M38.1321
10207 CONTINUE                                                          M32.2586
      IF ( BUS4  .EQ.  BLANK )   GO TO 10213                            M32.2587
      WRITE (LUNIT6, 13207)  BUS4                                       M32.2588
13207 FORMAT ( /,   36H ERROR STOP.   THE TACS NAME  "BUS4",            M32.2589
     1              25H ON THE LAST-READ CARD IS                        M32.2590
     2  ,/,         22H UNRECOGNIZED.   BUS4=,  A6  )                   M32.2591
      GO TO 9600                                                                
10209 NDX1 = KXTCS + J                                                  M32.2593
      JTMTAC(N1) = NDX1                                                 M32.2594
C     READ INPUT CARD USING CIMAGE                                      M32.2595
10213 CALL CIMAGE                                                       M32.2596
      DECODE (80, 14203, ABUFF(1))  OMEGM(N1), REAMDU(N1),              M32.2597
     1  JCDSAT(N1), REAMDS(N1), FLXDS(N1), FLXDR(N1)                    M32.2598
14203 FORMAT ( 2E14.5,  I1,  3E14.5 )                                   M32.2599
      IF ( NOUTPR  .EQ.  0 )                                            M32.2600
     1 WRITE (KUNIT6, 15203)  NUMUM, OMEGM(N1), REAMDU(N1)              M35. 957
15203 FORMAT(5H+UM -, I3, 16H   MACH CARD  2.,2E10.2)                   M32.2602
C     READ INPUT CARD USING CIMAGE                                      M32.2603
      CALL CIMAGE                                                       M32.2604
      DECODE (80, 14203, ABUFF(1))  THETAM(N1), REAMQU(N1),             M32.2605
     1  JCQSAT(N1), REAMQS(N1), FLXQS(N1), FLXQR(N1)                    M32.2606
      IF (REAMQU(N1) .GT. 0.0) GO TO 15910                              M36. 607
      WRITE (LUNIT6,6198) N1                                            M36. 608
      GO TO 9600                                                                
15910 IF ( NOUTPR  .EQ.  0 )                                            M36. 610
     1 WRITE (KUNIT6, 15903)  NUMUM, THETAM(N1), REAMQU(N1)             M36. 611
15903 FORMAT(5H+UM -, I3, 16H   MACH CARD  3.,2E10.2)                   M36. 612
      IF (IPRSUP .GE. 4) WRITE (LUNIT6,15912) N1                        M37.2571
15912 FORMAT (/,40H ***************************************,            M37.2572
     1         37H THE MACHINE-TABLE INPUT OF UM NUMBER,I4,2H :)        M37.2573
      IF ( IPRSUP  .GE.  4 )                                            M37.2574
     1 WRITE(LUNIT6,10214) JTYPE(N1),NCLD(N1),NCLQ(N1),JTQOUT(N1),      M32.2611
     VJOMOUT(N1),JTHOUT(N1), BUS3, BUS4, NPPAIR(N1),                    M32.2612
     VROTMOM(N1),DCOEF(N1),EPSOM(N1),OMEGM(N1),REAMDU(N1),              M32.2613
     VJCDSAT(N1),REAMDS(N1),FLXDS(N1),FLXDR(N1),THETAM(N1),             M32.2614
     VREAMQU(N1),JCQSAT(N1),REAMQS(N1),FLXQS(N1),FLXQR(N1)              M32.2615
10214 FORMAT(10H *********,3I2,3I1,2A6,I2,3E14.5/                       M37.2575
     1       10H *********,2E14.5,I1,3E14.5/                            M37.2576
     2       10H *********,2E14.5,I1,3E14.5)                            M37.2577
C  READ SHARE CARD(IF SHARING OF MECH NETW IS REQUESTED) :              M39.1167
      NSHARE = 0                                                                
      CALL CIMAGE                                                       M39.1169
      DECODE (80,15800,ABUFF(1)) TEXT3                                  M39.1170
15800 FORMAT(A6)                                                        M39.1171
      IF (TEXT3 .NE. TESM9) GO TO 15830                                 M39.1172
      NSHARE = 1                                                                
      DECODE (80,15810,ABUFF(1)) N5,N6                                  M39.1174
15810 FORMAT(6X,2I6)                                                    M39.1175
      IF (N6 .EQ. 0) GO TO 15822                                        M39.1176
      IF (NOUTPR .EQ. 0)                                                M39.1177
     1 WRITE(KUNIT6,15820) NUMUM,N5,N6                                  M39.1178
15820 FORMAT(5H+UM -,I3,26H SHARES MECH NETW WITH UM-,I3,3HAND,I3)      M39.1179
      GO TO 15826                                                       M39.1180
15822 IF (NOUTPR .EQ. 0)                                                M39.1181
     1 WRITE(KUNIT6,15824) NUMUM,N5                                     M39.1182
15824 FORMAT(5H+UM -,I3,26H SHARES MECH NETW WITH UM-,I3)               M39.1183
15826 IF (N1 .NE. N5 .AND. N1 .NE. N6) GO TO 15828                      M39.1184
      WRITE(LUNIT6,15827) N1                                            M39.1185
15827 FORMAT(/,21H ERROR STOP. THIS UM-,I3,15H IS SUPPOSED TO,          M39.1186
     1         45H SHARE ITS MECH NETWORK WITH OTHER UM'S. WHAT,        M39.1187
     2         41H SHOULD BE SPECIFIED ON THIS CARD ARE THE             M39.1188
     3  ,/,    36H NUMBERS OF THESE OTHER UM'S WITHOUT,                 M39.1189
     4         40H INCLUDING THE NUMBER OF THE UM WHICH IS,             M39.1190
     5         27H BEING PROCESSED RIGHT NOW.)                          M39.1191
      CALL STOPTP                                                       M39.1192
15828 N10 = N1                                                                  
      IF (N10 .GT. N5) N10 = N5                                         M39.1194
      IF (N6 .NE. 0 .AND. N10 .GT. N6) N10 = N6                         M39.1195
      IF (N1 .NE. N10) NSHARE = 2                                               
15830 IF (INITUM .EQ. 0) GO TO 10300                                    M39.1197
      IF (NSHARE .EQ. 0) GO TO 15880                                    M39.1198
      CALL CIMAGE                                                       M32.2619
15880 D1 = 1.0E+3                                                       M39.1199
C  MAKE SURE THAT USER SETS UP AUTOMATIC SS-CALC IF LDFLOW :            M39.1200
      IF (ISTEP .NE. -4567) GO TO 15890                                 M39.1201
      IF (NSHARE .EQ. 2) GO TO 15890                                    M39.1202
      DECODE(80,15882,ABUFF(1)) BUS6                                    M39.1203
15882 FORMAT(34X,A6)                                                    M39.1204
      IF (BUS6 .NE. BLANK) GO TO 15890                                  M39.1205
      WRITE(LUNIT6,80502)                                               M39.1206
      CALL STOPTP                                                       M39.1207
15890 N5 = 4                                                            M39.1208
      N6 = 0                                                            M32.2622
      DECODE (80,15904,ABUFF(1)) VOLTUM(N1), ANGLUM(N1),BUS5,           M36. 613
     1  BUS6, DISTRF                                                    M38.1323
15904 FORMAT(2E14.5, 2A6, E14.5)                                        M38.1324
      IF (NOUTPR .EQ. 0)                                                M32.2626
     1 WRITE (KUNIT6, 15905)  NUMUM, VOLTUM(N1), ANGLUM(N1)             M36. 615
15905 FORMAT(5H+UM -, I3, 16H   MACH CARD  4.,2E10.2)                   M36. 616
      IF (JTYPE(N1) .EQ. 3) GO TO 10230                                 M32.2629
      IF (JTYPE(N1) .EQ. 4) GO TO 10230                                 M32.2630
      IF (JTYPE(N1) .EQ. 5) GO TO 10230                                 M32.2631
      DO 10220 N10 = 1,NTOT                                             M32.2632
      IF (BUS5 .NE. BUS(N10)) GO TO 10220                               M32.2633
      NODFUM(N1) = N10                                                  M32.2634
      DO 10219 N11 = 1,KCONST                                           M32.2635
      N12 = NODE(N11)                                                   M32.2636
      IF (N12 .LT. 0) N12 = - N12                                       M32.2637
      IF (NODFUM(N1) .NE. N12) GO TO 10219                              M32.2638
      NODFUM(N1) = N11                                                  M32.2639
      GO TO 10230                                                       M32.2640
10219 CONTINUE                                                          M32.2641
10220 CONTINUE                                                          M32.2642
      WRITE (LUNIT6, 10222)  BUS5                                       M32.2643
10222 FORMAT ( /,   36H ERROR STOP.   THE NODE NAME  "BUSF",            M32.2644
     1              34H ON THE LAST-READ CARD IS NOT THAT               M32.2645
     2  ,/,         37H OF ANY ELECTRIC NETWORK SOURCE NODE.,           M32.2646
     3               6H BUSF=,  A6  )                                   M32.2647
      GO TO 9600                                                                
10230 DO 10233 N10 = 1,NTOT                                             M32.2649
      IF (NSHARE .EQ. 2 .AND. BUS6 .EQ. BLANK) GO TO 10250              M39.1209
      IF (BUS6 .NE. BUS(N10)) GO TO 10233                               M32.2650
      DO 10232 N11 = 1,KCONST                                           M32.2651
      N12 = NODE(N11)                                                   M32.2652
      IF (N12 .LT. 0) N12 = - N12                                       M32.2653
      IF (N10 .NE. N12) GO TO 10232                                     M32.2654
      CREST(N11) = D1 * DISTRF                                          M32.2655
      IF (N6 .NE. 0) GO TO 10231                                        M32.2656
      NODMUM(N1) = N11                                                  M32.2657
      IF (DISTRF .EQ. 0.0) CREST(N11) = D1                              M32.2658
10231 TSTART(N11) = - 7777.                                             M32.2659
      GO TO 10250                                                       M32.2660
10232 CONTINUE                                                          M32.2661
10233 CONTINUE                                                          M32.2662
      IF (NSHARE .NE. 2) GO TO 90233                                    M39.1210
      WRITE(LUNIT6,90200) BUS6                                          M39.1211
90200 FORMAT(/,28H ERROR STOP. THE NODE NAME ",A6, 8H" ON THE,          M39.1212
     1         37H LAST-READ CARD SHOULD BE LEFT BLANK.)                M39.1213
      WRITE (LUNIT6,90202) N1                                           M39.1214
90202 FORMAT(10H THIS UM -,I3,26H IS REQUESTED TO SHARE ITS,            M39.1215
     1       41H MECHANICAL NETWORK WITH OTHER UM'S.  THE,              M39.1216
     2       45H SPECIFICATION OF ALL APPLIED TORQUES IN THIS           M39.1217
     3   ,/, 42H MECH NETWORK SHOULD HAVE BEEN PLACED WITH,             M39.1218
     4       45H THE DATA INPUT FOR THE LOWEST-NUMBERED UM OF,          M39.1219
     5       43H THE SET OF UM'S SHARING THIS MECH NETWORK.)            M39.1220
      CALL STOPTP                                                       M39.1221
90233 WRITE (LUNIT6, 10234) BUS6                                        M39.1222
10234 FORMAT ( /,   36H ERROR STOP.   THE NODE NAME  "BUSM",            M32.2664
     1              34H ON THE LAST-READ CARD IS NOT THAT               M32.2665
     2  ,/,         37H OF ANY ELECTRIC NETWORK SOURCE NODE.,           M32.2666
     3               6H BUSM=,  A6  )                                   M32.2667
      GO TO 9600                                                                
10250 CALL CIMAGE                                                       M32.2669
      DECODE(80,10260,ABUFF(1)) TEXT3                                   M32.2670
10260 FORMAT(A6)                                                        M32.2671
      IF (TEXT3 .NE. TEXTM) GO TO 10300                                 M38.1325
      IF (NSHARE .NE. 2) GO TO 90260                                    M39.1223
      WRITE(LUNIT6,90250) N1                                            M39.1224
90250 FORMAT(/,43H ERROR STOP. THIS CARD 5 OF CLASS 2 UM DATA,          M39.1225
     1         19H CARDS OF THIS UM -,I3,19H SHOULD BE REMOVED.)        M39.1226
      WRITE(LUNIT6,90202) N1                                            M39.1227
      CALL STOPTP                                                       M39.1228
90260 N5 = N5 + 1                                                       M39.1229
      N6 = 1                                                            M32.2674
      DECODE (80,10261,ABUFF(1)) TEXT3, BUS6, DISTRF                    M32.2675
10261 FORMAT(A6, 28X, A6, E14.5)                                        M32.2676
      IF (NOUTPR .EQ. 0)                                                M32.2677
     1 WRITE (KUNIT6, 10262)  NUMUM, N5, DISTRF                         M35. 958
10262 FORMAT( 5H+UM -, I3, 12H   MACH CARD, I3, 1H., E14.5)             M32.2679
      GO TO 10230                                                       M32.2680
C. IF FLUX OUTPUT IS DESIRED :                                          M32.2681
C. USER3 : COIL-TABLE INPUT FOR M32 AND NEWER                           M32.2684
10300 IF ( IPRSUP  .GE.  4 ) WRITE(LUNIT6,10301) NUMUM                  M37.2580
10301 FORMAT(/,40H ***************************************,             M37.2581
     1         34H THE COIL-TABLE INPUT OF UM NUMBER,I4,2H :)           M37.2582
      N8 = 0                                                                    
      NCLDQ = NCLD(N1) + NCLQ(N1) + 3                                   M32.2688
C. ADJUSTMENT OF NCLD AND NCLQ FOR UM TYPE 4 :                          M32.2689
      IF (JTYPE(N1) .NE. 4) GO TO 10304                                 M32.2690
      NCLD(N1) = 1                                                      M32.2691
      NCLQ(N1) = 1                                                      M32.2692
      NCLDQ = NCLD(N1) + NCLQ(N1) + 4                                   M32.2693
10304 IF (INITUM .NE. 0) GO TO 14303                                    M32.2694
13302 IF (N8 .GE. NCLDQ) GO TO 11202                                    M32.2695
      IF (N8 .EQ. 0 .AND. NSHARE .EQ. 0) GO TO 14303                    M39.1231
      CALL CIMAGE                                                       M32.2696
14303 NCLTOT = NCLTOT + 1                                               M32.2697
      FPAR(NCLTOT) = 0.0                                                M38.1326
      IF ( NCLTOT  .LE.  NCLFIX )   GO TO 14305                         M32.2698
      WRITE (LUNIT6, 14304)  NCLTOT                                     M32.2699
14304 FORMAT ( /,   26H OVERFLOW U.M. COIL TABLE.,                      M32.2700
     1              11H   NCLTOT =,  I5  )                              M32.2701
      GO TO 9600                                                                
14305 N8 = N8 + 1                                                       M32.2703
      DECODE (80, 13303, ABUFF(1))  GPAR(NCLTOT), REACL(NCLTOT),        M32.2704
     1  BUS1, BUS2, BUS6, JCLOUT(NCLTOT), HIST(NCLTOT)                  M32.2705
13303 FORMAT (2E14.5, 3A6, I1, E14.5)                                   M32.2706
      IF ( NOUTPR  .EQ.  0 )                                            M32.2707
     1 WRITE (KUNIT6, 14306)                                            M35. 959
     2            NUMUM, N8, GPAR(NCLTOT), REACL(NCLTOT)                M32.2709
14306 FORMAT(5H+UM -, I3, 12H   COIL CARD, I3, 1H.,2E11.2)              M32.2710
      N11 = 0                                                           M32.2711
      N12 = 0                                                           M32.2712
      DO 13306 N10 = 1,NTOT                                             M32.2713
      IF (N11 .NE. 0) GO TO 13304                                       M32.2714
      IF (BUS1 .NE. BUS(N10)) GO TO 13304                               M32.2715
      NODVO1(NCLTOT) = N10                                              M32.2716
      N11 = 1                                                           M32.2717
      IF (N12 .NE. 0) GO TO 13305                                       M32.2718
13304 IF (BUS2 .NE. BUS(N10)) GO TO 13305                               M32.2719
      NODVO2(NCLTOT) = N10                                              M32.2720
      N12 = 1                                                           M32.2721
13305 N13 = N11 * N12                                                   M32.2722
      IF (N13 .NE. 0) GO TO 13307                                       M32.2723
13306 CONTINUE                                                          M32.2724
      IF ( N11  .EQ.  0 )                                               M32.2725
     1 WRITE (LUNIT6, 13206)  BUS1                                      M32.2726
13206 FORMAT ( /,    33H ERROR STOP.  JUST-READ COIL CARD,              M32.2727
     1               25H BEARS ILLEGAL NODE NAME:,  A6  )               M32.2728
      IF ( N12  .EQ.  0 )                                               M32.2729
     1 WRITE (LUNIT6, 13206)  BUS2                                      M32.2730
      GO TO 9600                                                                
13307 JCLTAC(NCLTOT) = 0                                                M32.2732
      IF (BUS6 .EQ. BLANK) GO TO 13313                                  M39.1232
      DO 13309 J=1,KTAB                                                 M32.2733
      NDX1 = ILNTAB( KLNTAB + J )                                       M38.1327
      IF (BUS6 .EQ. TEXVEC(NDX1)) GO TO 13311                           M38.1328
13309 CONTINUE                                                          M32.2736
      WRITE (LUNIT6, 13207)  BUS6                                       M32.2738
      GO TO 9600                                                                
13311 NDX1 = KXTCS + J                                                  M32.2740
      JCLTAC(NCLTOT) = NDX1                                             M39.1233
13313 IF (IPRSUP .GE. 4)                                                M37.2586
     1 WRITE(LUNIT6, 13314) NCLTOT,GPAR(NCLTOT),REACL(NCLTOT),          M37.2587
     1 BUS1, BUS2, BUS6, JCLOUT(NCLTOT),HIST(NCLTOT)                    M37.2588
13314 FORMAT(23H ******** COIL CARD NR.,I3,2H :,2E14.5,3A6,I1,E14.5)    M37.2589
      GO TO 13302                                                       M32.2746
C.  AT THIS POINT READING OF ALL UM DATA INPUT IS FINISHED              M32.2747
C         ******************************************************        M32.2748
C. CALCULATION OF REAL GPAR :                                           M32.2749
  500 DO 502 N1 = 1,NCLTOT                                              M32.2750
      IF (GPAR(N1) .NE. 0.0) GPAR(N1) = 1.0/GPAR(N1)                    M32.2751
  502 CONTINUE                                                          M32.2752
      IF (ISTEP .NE. -4567) GO TO 80510                                 M39.1234
      IF (INITUM .EQ. 1) GO TO 80510                                    M39.1235
      WRITE(LUNIT6,80502)                                               M39.1236
80502 FORMAT(/,41H ERROR STOP. YOU HAVE REQUESTED THE UM TO,            M39.1237
     1         44H BE INITIALIZED THROUGH A LOAD-FLOW PROCESS.,         M39.1238
     2         41H YOU HAVE HOWEVER FAILED TO SET UP THE UM             M39.1239
     3    ,/,  38H DATA INPUT FOR AUTOMATIC STEADY-STATE,               M39.1240
     4         43H INITIALIZATION. CONSULT THE EMTP RULE BOOK,          M39.1241
     5         32H REGARDING THIS USAGE OF THE UM.)                     M39.1242
      CALL STOPTP                                                       M39.1243
80510 IF (NSMACH .EQ. 0) GO TO 510                                      M39.1244
C  WARNING IF OVERFLOW OF ETAC,ISMTAC AND BUSTAC                        M32.2753
      N1 = NTOTAC - NTACB                                               M32.2754
      IF (N1 .LE. 10) GO TO 510                                         M32.2755
      WRITE (LUNIT6,504) N1                                             M32.2756
  504 FORMAT(/, 40H WARNING: IN USING SM TYPE-50 TO 59 DATA,            M32.2757
     1          36H INPUT,YOU HAVE REQUESTED A TOTAL OF, I4)            M32.2758
      WRITE (LUNIT6,505)                                                M32.2759
  505 FORMAT(/, 34H TO BE PASSED TO TACS. THE DEFAULT,                  M32.2760
     1          37H STORAGE ALLOCATION IS JUST FOR 10 OF)               M32.2761
      WRITE (LUNIT6, 506)                                               M32.2762
  506 FORMAT(/, 38H SUCH VARIABLES BECAUSE IN DECK SYNMAC,              M32.2763
     1          36H THE ARRAYS ISMTAC, ETAC, BUSTAC ARE)                M32.2764
      WRITE (LUNIT6, 507)                                               M32.2765
  507 FORMAT(/, 41H DIMENSIONED TO 10. DID YOU INCREASE THIS,           M32.2766
     1          39H DIMENSION TO BE AT LEAST EQUAL TO THIS)             M32.2767
      WRITE (LUNIT6, 508)                                               M32.2768
  508 FORMAT(/, 43H TOTAL NR OF VARIABLES YOU WANT TO TRANSFER,         M32.2769
     1          27H TO TACS?  YOU SHOULD HAVE.    )                     M32.2770
  510 KCOIL(1) = 1                                                      M32.2771
      NUMOUT = 0                                                        M32.2772
      IF (LOOPSS(8) .NE. 1) GO TO 530                                   M33. 296
      N1 = 6 * NUMUM                                                    M33. 297
      IF (N1 .LE. NCLFIX) GO TO 530                                     M33. 298
      WRITE (LUNIT6,520)                                                M33. 299
  520 FORMAT( /, 28H OVERFLOW OF UM COIL TABLE, ,                       M33. 300
     1           32H INCREASE NCLFIX ON UM DIMENSION,                   M33. 301
     2           32H CARD TO BE GREATER THAN 6 TIMES,                   M33. 302
     3           35H THE TOTAL NR. OF USED UM MACHINES.)                M33. 303
      GO TO 9600                                                                
C. MACHINE LOOPING FOR SPECIAL PROCESSING OF SOME PARAMETERS:           M32.2773
  530 DO 1050 N1 = 1,NUMUM                                                      
      NUMOUI = NUMOUT                                                           
      RPPAIR = NPPAIR(N1)                                               M32.2775
      IF (JTYPE(N1) .EQ. 13) JTMTAC(N1) = 0                             M38.1331
      IF (INITUM .EQ. 1) OMEGM(N1) = ROTMOM(N1)/RPPAIR                  M37.2591
      OMOLD(N1) = OMEGM(N1)                                             M36. 618
      FLXD(N1) = 0.0                                                    M28.4137
      FLXQ(N1) = 0.0                                                    M28.4138
C. OMOLD INITIALIZATION, TORQUE ANGLE FOR TYPE 1 AND 1,                 M28.4148
C. AND COIL ADJUSTMENT FOR THREE-PHASE EXC. COILS :                     M28.4149
      IF (JTYPE(N1) .GT. 2) GO TO 921                                   M28.4152
      THETAM(N1) = (THETAM(N1) + PI/2.0)/RPPAIR                         M32.2781
C. SET UP OF ARRAY KCOIL(1:NUMUM) :                                     M28.4156
  921 IF (N1 .EQ. NUMUM) GO TO 950                                      M39.1245
      N2 = N1 + 1                                                       M32.2782
      N3 = 3 + NCLD(N1) + NCLQ(N1)                                      M28.4160
      IF (JTYPE(N1) .NE. 4) GO TO 940                                   M28.4161
      N3 = N3 + 1                                                       M28.4162
  940 KCOIL(N2) = KCOIL(N1) + N3                                        M28.4163
  950 KCL = KCOIL(N1)                                                   M39.1246
C  MAKING SURE THAT POWER COILS ARE NOT TACS CONTROLLED :               M39.1247
      DO 958 N2 = 1,3                                                   M39.1248
      N3 = KCL - 1 + N2                                                 M39.1249
      IF (JCLTAC(N3) .EQ. 0) GO TO 958                                  M39.1250
      WRITE(LUNIT6,952) N1                                              M39.1251
  952 FORMAT(/,22H ERROR STOP. UM NUMBER, I4, 12H IS PROVIDED,          M39.1252
     1         42H WITH TACS CONTROLLED SOURCES ON THE POWER,           M39.1253
     2         44H SIDE. THIS IS ONLY ALLOWED FOR THE COILS ON          M39.1254
     3   ,/,   43H THE EXCITATION SIDE. TACS CONTROL OF POWER,          M39.1255
     4         45H SIDE COILS IS TO BE DONE THROUGH THE NETWORK,        M39.1256
     5         35H WHICH IS CONNECTED TO THESE COILS.)                  M39.1257
      CALL STOPTP                                                       M39.1258
  958 CONTINUE                                                          M39.1259
      IF (INITUM .EQ. 0) GO TO 990                                      M39.1260
      DO 980 N2 = 1,3                                                   M36. 625
      N3 = KCL - 1 + N2                                                 M36. 626
C  PREPARING SHIFT OF LEAKAGE INDUCTANCES OF POWER COILS *******        M39.1261
C  NOTE : THE EMTP ADDRESS IT OF THESE SHIFTED INDUCTANCES              M38.1343
C         ARE STORED IN FPAR(KCL).                                      M38.1344
  961 IF (NODVO1(N3) .EQ. NODVO2(N3)) GO TO 980                         M38.1345
C  CREATE NEW NODE BEHIND LEAKAGE INDUCTANCE. THIS NODE                 M36. 628
C    BECOMES THE NEW UM POWER NODE.                                     M36. 629
      NTOT = NTOT + 1                                                   M36. 630
      KODE(NTOT) = 0                                                    M36. 631
      BUS(NTOT) = TRASH                                                 M36. 632
      IF (N1 .GT. 5) GO TO 962                                          M36. 633
      N4 = 75 + (N1-1)*3 + N2                                           M36. 634
      BUS(NTOT) = TEXTA(N4)                                             M36. 635
C  CREATION OF THE LEAKAGE INDUCTANCE BRANCHE                           M36. 636
  962 CALL IBRINC                                                       VAX.  49
      IT = IT + 1                                                       M36. 638
      LENGTH(IBR) = 1                                                   M36. 639
      NR(IBR) = - IT                                                    M36. 640
      TR(IT) = 0.0                                                      M36. 641
      C(IT) = 0.0                                                       M36. 642
      D1 = REACL(KCL+2)                                                 M36. 643
      IF (D1 .GT. REACL(KCL+1)) D1 = REACL(KCL+1)                       M36. 644
      IF (JTYPE(N1) .LT. 3) GO TO 966                                   M38.1346
      IF (JTYPE(N1) .GT. 5) GO TO 966                                   M38.1347
      IF (N2 .NE. 3) GO TO 966                                          M38.1348
      REACL(KCL+1) = REACL(KCL+1) - D1                                  M38.1349
      REACL(KCL+2) = REACL(KCL+2) - D1                                  M38.1350
  966 IF (XOPT .EQ. 0.0) TX(IT) = D1 * 1.0D+3                           M36. 651
      IF (XOPT .NE. 0.0) TX(IT) = D1 * TWOPI*XOPT                       M38.1351
      IF (N3 .EQ. KCL) FPAR(KCL) = IBR                                  M37.2596
      N4 = 3 * (N1 - 1)                                                 M37.2597
      IF (NODVO1(N3) .EQ. 1) GO TO 972                                  M36. 654
      UMCURP(N4+N2) = NODVO1(N3)                                        M37.2598
      KBUS(IBR) = NODVO1(N3)                                            M36. 655
      MBUS(IBR) = NTOT                                                  M36. 656
      NODVO1(N3) = NTOT                                                 M36. 657
      GO TO 974                                                         M36. 658
  972 UMCURP(N4+N2) = NODVO2(N3)                                        M37.2599
      KBUS(IBR) = NODVO2(N3)                                            M37.2600
      MBUS(IBR) = NTOT                                                  M36. 660
      NODVO2(N3) = NTOT                                                 M36. 661
  974 IF (N2 .NE. 1) GO TO 976                                          M36. 662
      IF (IPRSUP .GE. 1) WRITE(LUNIT6,975) N1                           M36. 663
  975 FORMAT( /, 36H ADDITIONAL BRANCHES CREATED TO MOVE,               M36. 664
     1           38H LEAKAGE INDUCT TO EMTP NETWORK FOR UM,             M36. 665
     2            7H NUMBER,I3,1H.,/,21X,12HNODE TO NODE,5X,            M36. 666
     3            3HIBR,6X,2HIT,8X,6HTX(IT))                            M37.2601
  976 IF (IPRSUP .GE. 1)                                                M36. 668
     1 WRITE (LUNIT6,977) MBUS(IBR),KBUS(IBR),IBR,IT,TX(IT)             M37.2602
  977 FORMAT( 21X,I4,4X,I4,4X,I4,4X,I4,E14.5)                           M37.2603
  980 CONTINUE                                                          M36. 671
C  CREATION OF SPEED CAPACITORS FOR STEADY-STATE INITIALIZATION:        M39.1262
      NTOT = NTOT + 1                                                           
      IF (JTMTAC(N1) .LE. 0) JTMTAC(N1) = - NTOT                                
      BUS(NTOT) = TRASH                                                 M38.1355
      IF (N1 .LT. 4) BUS(NTOT) = TEXTA(98+N1)                           M38.1356
      CALL IBRINC                                                       VAX.  50
      IT = IT + 1                                                       M38.1358
      FPAR(KCL+2) =  IT                                                         
      LENGTH(IBR) = 1                                                   M38.1359
      NR(IBR) = - IT                                                    M38.1360
      KBUS(IBR) = NODOM(N1)                                             M38.1361
      MBUS(IBR) = NTOT                                                  M38.1362
      TR(IT) = EPSILN                                                           
      C(IT) = 0.0                                                               
      TX(IT) = 0.0                                                      M38.1365
      IF (IPRSUP .GE. 1)                                                M38.1366
     1 WRITE (LUNIT6,80980) MBUS(IBR),KBUS(IBR),IBR,IT,                 M38.1367
     2                      TR(IT),C(IT)                                M38.1368
80980 FORMAT(10H *********,                                             M38.1369
     1       29H POTENTIAL SPEED CAP : MBUS =,I6, 7H KBUS =,I6,         M38.1370
     2        6H IBR =,I6, 5H IT =,I6, 9H TR(IT) =,E14.5,               M38.1371
     3        8H C(IT) =,E14.5)                                         M38.1372
C  CREATION OF SOURCES TO REPRESENT IM EXCITATION COILS FOR             M37.2604
C    STEADY-STATE INITIALIZATION (ACCOMMODATES KPSOUR USE):             M37.2605
      IF (JTYPE(N1) .LT. 3) GO TO 990                                   M37.2607
      IF (JTYPE(N1) .GT. 7) GO TO 990                                   M37.2608
      N5 = KCL + 3                                                      M37.2609
      N6 = KCL + 2 + NCLD(N1) + NCLQ(N1)                                M37.2610
      IF (JTYPE(N1) .EQ. 4) N6 = N6 + 1                                 M37.2611
      N11 = KCONST + 1                                                  M37.2612
      DO 988 J = N5,N6                                                  M37.2613
      IF (NODVO1(J) .EQ. NODVO2(J)) GO TO 988                           M37.2614
      DO 987 N10 = 1,2                                                  M37.2615
      IF (N10 .EQ. 2) GO TO 982                                         M37.2616
      IF (NODVO1(J) .EQ. 1) GO TO 987                                   M37.2617
      KCONST = KCONST + 1                                               M37.2618
      N20 = NODVO1(J)                                                   M37.2619
      NODE(KCONST) = - N20                                              M37.2620
      KODE(N20) = 0                                                     M37.2621
      GO TO 984                                                         M37.2622
  982 IF (NODVO2(J) .EQ. 1) GO TO 987                                   M37.2623
      KCONST = KCONST + 1                                               M37.2624
      N20 = NODVO2(J)                                                   M37.2625
      NODE(KCONST) = - N20                                              M37.2626
      KODE(N20) = 0                                                     M37.2627
  984 IF (KCONST .EQ. N11) FPAR(KCL+1) = KCONST                         M37.2628
      IFORM(KCONST) = 14                                                M37.2629
      CREST(KCONST) = 0.0                                               M37.2630
      TIME1(KCONST) = 0.0                                               M37.2631
      TSTART(KCONST) = - 1.0                                            M37.2632
      TSTOP(KCONST) = 0.0                                               M37.2633
      SFREQ(KCONST) = 0.0001                                            M37.2634
      IF (IPRSUP .GE. 1 .AND. J .EQ. N5) WRITE(LUNIT6,985)              M37.2635
  985 FORMAT(10H *********,                                             M37.2636
     1       47H STEADY-STATE CURR SOURCES FOR IM EXCIT COILS :,        M37.2637
     2       44H    NODE  KCONST         SFREQ         CREST,           M37.2638
     3       14H         TSTOP)                                         M37.2639
      IF (IPRSUP .GE. 1)                                                M37.2640
     1 WRITE (LUNIT6,986) NODE(KCONST),KCONST,SFREQ(KCONST),            M37.2641
     2                    CREST(KCONST),TSTOP(KCONST)                   M37.2642
  986 FORMAT(10H *********,47X,2I8,3E14.5)                              M37.2643
  987 CONTINUE                                                          M37.2644
  988 CONTINUE                                                          M37.2645
  990 N2 = KCOIL(N1)                                                    M36. 672
      N3 = N2 + NCLD(N1) + NCLQ(N1) + 2                                 M36. 673
      IF (JTYPE(N1) .EQ. 4) N3 = N3 + 1                                 M36. 674
      IF (IPRSUP .GE. 1) WRITE(LUNIT6,992) N1                           M36. 675
  992 FORMAT(/, 35H ELECTRIC TERMINAL NODES FOR UM NR.,I3,2H: )         M36. 676
      IF (IPRSUP .GE. 1)                                                M36. 677
     1 WRITE (LUNIT6,993) (NODVO1(N4),N4=N2,N3)                         M36. 678
      IF (IPRSUP .GE. 1)                                                M36. 679
     1 WRITE (LUNIT6,994) (NODVO2(N4),N4=N2,N3)                         M36. 680
  993 FORMAT( 9H NODVO1 :,3X,15I4)                                      M36. 681
  994 FORMAT( 9H NODVO2 :,3X,15I4)                                      M36. 682
      IF (IPRSUP .GE. 1)                                                M36. 683
     1 WRITE (LUNIT6,995) NODOM(N1),NODMUM(N1),NODFUM(N1)               M36. 684
  995 FORMAT(/, 31H UM MASS NODE NR. = NODOM(JM) =,I4                   M36. 685
     1  ,/, 14H SLACK BUSES :,3X,12HNODMUM(JM) =,I4,10X,                M36. 686
     3                           12HNODFUM(JM) =,I4)                    M36. 687
C. DETERMINATION OF POINTERS FOR OUTPUT VECTOR :                        M28.4172
      IF (JTQOUT(N1) .EQ. 0) GO TO 1000                                 M36. 688
      NUMOUT = NUMOUT + 1                                               M28.4176
      KUMOUT(NUMOUT) = N1                                               M28.4177
      JUMOUT(NUMOUT) = 1 + NUMFIX                                               
      IF (JTQOUT(N1) .LT. 2) GO TO 1000                                 M38.1389
      NUMOUT = NUMOUT + 1                                               M38.1390
      KUMOUT(NUMOUT) = N1                                               M38.1391
      JUMOUT(NUMOUT) = 4 + NUMFIX                                               
      IF (JTQOUT(N1) .NE. 3) GO TO 1000                                 M38.1393
      NUMOUT = NUMOUT + 1                                               M38.1394
      KUMOUT(NUMOUT) = N1                                               M38.1395
      JUMOUT(NUMOUT) = 5 + NUMFIX                                               
 1000 IF (JOMOUT(N1) .EQ. 0) GO TO 1010                                 M28.4179
      NUMOUT = NUMOUT + 1                                               M28.4180
      KUMOUT(NUMOUT) = N1                                               M28.4181
      JUMOUT(NUMOUT) = 2 + NUMFIX                                               
      IF (JOMOUT(N1) .LT. 2) GO TO 1010                                 M38.1398
      NUMOUT = NUMOUT + 1                                               M38.1399
      KUMOUT(NUMOUT) = N1                                               M38.1400
      JUMOUT(NUMOUT) = 6 + NUMFIX                                               
      IF (JOMOUT(N1) .NE. 3) GO TO 1010                                 M38.1402
      NUMOUT = NUMOUT + 1                                               M38.1403
      KUMOUT(NUMOUT) = N1                                               M38.1404
      JUMOUT(NUMOUT) = 7 + NUMFIX                                               
 1010 IF (JTHOUT(N1) .EQ. 0) GO TO 1020                                 M38.1406
      NUMOUT = NUMOUT + 1                                               M28.4184
      KUMOUT(NUMOUT) = N1                                               M28.4185
C  IF OUTPUT REQUEST FOR THETAM (TORQUE ANGLE OR ROTOR POSITION)        M38.1407
      IF (JTHOUT(N1) .EQ. 1) JUMOUT(NUMOUT) = 3 + NUMFIX                M38.1408
C  IF OUTPUT REQUEST FOR TQEXC (FOR SM TYPE-59 DATA INPUT)              M38.1409
      IF (JTHOUT(N1) .EQ. 3) JUMOUT(NUMOUT) = 11 + NUMFIX               M38.1410
 1020 N2 = 3 + NCLD(N1) + NCLQ(N1)                                      M28.4187
      IF (JTYPE(N1) .NE. 4) GO TO 1030                                  M28.4188
      N2 = N2 + 1                                                       M28.4189
 1030 DO 1040 N3=1,N2                                                   M28.4190
      N4 = KCOIL(N1) - 1 + N3                                           M28.4191
      IF (REACL(N4) .GE. 0.0) GO TO 1038                                M36. 689
      WRITE (LUNIT6, 1032) N1                                           M36. 690
 1032 FORMAT( /, 42H ERROR STOP. A NEGATIVE LEAKAGE INDUCTANCE,         M36. 691
     1           39H INDUCTANCE IS USED TO SIMULATE MACHINE,            M36. 692
     2            7H NUMBER, I4,1H.)                                    M36. 693
      GO TO 9600                                                                
 1038 IF (JCLOUT(N4) .EQ. 0) GO TO 1040                                 M36. 695
      NUMOUT = NUMOUT + 1                                               M28.4193
      KUMOUT(NUMOUT) = N1                                               M28.4194
      JUMOUT(NUMOUT) = 11 + NUMFIX + N3                                         
      IF (JCLOUT(N4) .NE. 2) GO TO 1040                                 M38.1412
      IF (N3 .EQ. 1) JUMOUT(NUMOUT) = NUMFIX + 8                                
      IF (N3 .EQ. 2) JUMOUT(NUMOUT) = NUMFIX + 9                                
      IF (N3 .EQ. 3) JUMOUT(NUMOUT) = NUMFIX + 10                               
 1040 CONTINUE                                                          M28.4196
      IF (NUMOUT .EQ. NUMOUI) GO TO 1050                                M38.1416
      IF (N1 .EQ. 1) NUMBUS = JUMOUT(NUMOUT)                            M38.1417
      N11 = JUMOUT(NUMOUT)                                              M38.1418
      IF (N11 .GT. NUMBUS) NUMBUS = N11                                 M38.1419
 1050 CONTINUE                                                          M28.4203
      IF (NUMBUS .LE. IBSFIX) GO TO 1060                                M33. 308
      WRITE (LUNIT6,1055)                                               M33. 309
 1055 FORMAT( /, 35H OVERFLOW OF UM OUTPUT NAME TABLE, ,                M33. 310
     1           38H INCREASE IBSFIX ON UM DIMENSION CARD.)             M33. 311
      GO TO 9600                                                                
C  SHIFTING ENTRIES OF UMOUTP VECTOR BECAUSE OF SM TYPE-59              M32.2786
C    REQUEST FOR TRANSFER OF VARIABLES TO TACS :                        M32.2787
 1060 IF (NSMACH .EQ. 0) GO TO 18010                                    M36. 696
      IF (NSMTAC .EQ. 0) GO TO 18010                                    M32.2789
      DO 18000 N5 = 1,NSMTAC                                            M32.2790
      N6 = NSMTAC + 1 - N5                                              M32.2791
      N7 = NUMOUT + 3 + N6                                              M32.2792
      UMOUTP(N7) = UMOUTP(N6)                                           M32.2793
18000 CONTINUE                                                          M32.2794
      DO 18002 N5 = 1,NUMOUT                                            M32.2795
18002 UMOUTP(N5) = 0.0                                                  M32.2796
      N5 = NUMOUT + NSMTAC + 3                                          M36. 697
      UMOUTP(NUMOUT+3) = N5                                             M36. 698
C  UMOUTP(NUMOUT+3) IS LAST ENTRY OF UMOUTP THAT IS USED                M32.2798
      UMOUTP(NUMOUT+2) = NANGRE                                         M32.2799
C  UMOUTP(NUMOUT+2) .NE. 0.0 INDICATES REQUEST FOR ANGLE TRANSF.        M32.2800
      UMOUTP(NUMOUT+1) = - 9999.0                                       M32.2801
C  UMOUTP(NUMOUT+1) .EQ. -9999. IS A FLAG FOR REQUEST OF TACS           M32.2802
C  TRANSFER OF UM VARIABLES.                                            M32.2803
      IF (IPRSUP .GE. 1)                                                M36. 699
     1 WRITE (LUNIT6,18006) (UMOUTP(N1),N1=1,N5)                        M36. 700
18006 FORMAT(/, 45H THE UM OUTPUT TABLE UMOUTP(NUMOUT+NSMTAC+3):        M36. 701
     1    ,/, 6(6X,E14.5)/(6(6X,E14.5)) )                               M36. 702
18010 ISTART = 0                                                        M32.2804
      IF (NUMOUT .LE. IOTFIX) GO TO 18020                               M32.2805
      WRITE (LUNIT6, 17960)                                             M32.2806
17960 FORMAT( /, 38H OVERFLOW OF UM OUTPUT TABLE, INCREASE,             M42.3102
     1           35H IN CARD FOR ABSOLUTE UM DIMENSIONS                 M42.3103
     2  ,/,      17H VALUE OF IOTFIX.                                   M42.3104
     3  ,/,      37H REMARK : IF SM TYPE-59 DATA INPUT IS,              M42.3105
     4           30H INCLUDED, THEN IOTFIX IS ALSO                      M42.3106
     5  ,/,      36H RELATED TO OUTPUTS OF MECH. SYSTEMS,               M42.3107
     6           33H IN ALL SM TYPE-59 DATA MACHINES.)                  M42.3108
      GO TO 9600                                                                
18020 LOOPSS(1) = 0                                                     M32.2808
      LOOPSS(2) = 0                                                     M31. 866
      LOOPSS(4) = 0                                                     M31. 868
      LOOPSS(10) = 0                                                    M37.2649
      IF (IPRSUP .GE. 1)                                                M36. 703
     1 WRITE (LUNIT6,18040) NUMUM,NUMOUT,NSMTAC                         M36. 704
18040 FORMAT(/, 40H EXIT  "UMDATA" :  NUMUM  NUMOUT  NSMTAC             M36. 705
     1    ,/,17X,3I8)                                                   M36. 706
      RETURN                                                            M28.4209
 9600 CALL STOPTP                                                               
      RETURN                                                                    
      END                                                               M28.4210
      SUBROUTINE UMDATB (REACL,GPAR,FPAR,NODVO1,NODVO2,JCLTAC,          M42.3109
     1 JTYPE,NODOM,NCLD,JTMTAC,REAMDU,REAMDS,FLXDS,FLXDR,REAMQU,        M42.3110
     2 FLXQS,FLXQR,JCDSAT,JCQSAT,NPPAIR,ROTMOM,NCLQ,JTQOUT,             M42.3111
     3 JTHOUT,REAMQS,VOLTUM,ANGLUM,NODFUM,NODMUM,KUMOUT,JUMOUT,         M42.3112
     4 JCLOUT, DCOEF, JOMOUT, UMOUTP )                                  M42.3113
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
       DIMENSION NODVO2(1), JTMTAC(1), FLXQS(1), ROTMOM(1)              M42.3114
       DIMENSION REACL(1),GPAR(1),FPAR(1),NODVO1(1),NODFUM(1)           M42.3115
       DIMENSION JCLTAC(1), JCLOUT(1), JTYPE(1), NODOM(1)               M42.3116
       DIMENSION REAMDS(1), FLXDS(1), FLXDR(1), REAMQU(1)               M42.3117
       DIMENSION FLXQR(1), JCDSAT(1), JCQSAT(1), NPPAIR(1)              M42.3118
       DIMENSION NCLD(1), NCLQ(1), JTQOUT(1),JOMOUT(1),JTHOUT(1)        M42.3119
       DIMENSION REAMQS(1),DCOEF(1),VOLTUM(1),ANGLUM(1)                 M42.3120
       DIMENSION NODMUM(1), KUMOUT(1), JUMOUT(1), UMOUTP(1)             M42.3121
       DIMENSION REAMDU(1)                                              M42.3122
       COMMON /UMLOCAL/ TEXTA, D1,D2,D3,D17, STAT59, FMUM, RMVAUM,
     1 RKVUM, S1UM, S2UM, ZLSBUM, S1QUM, S2QUM, AGLQUM, RAUM, XDUM,
     2 SQUM, XDPUM, XQPUM, XDPPUM, XQPPUM, TDPUM, TDPPUM, X0UM, RNUM,
     3 XNUM, XFUM, XDFUM, XDKDUM, XKDUM, XKQUM, XQKQUM, XGKQUM, XQUM,
     4 XQGUM, XGUM, DISTRF, HJUM, DSYNUM, DMUTUM, SPRING, DABSUM, 
     5 TQPPUM, AGLDUM, XLUM, N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,
     1 N14,N15,N16,N17,N18,N19,N20, JR,JF, NEXC, KCONEX, IBREXC,
     2 NSTAN, NUMASU, NMGEN, NMEXC, NTYPSM, NETRUN, NETRUM, NSMTPR,
     3 NSMTAC, NRSYN, NTORQ, MLUM, NPARUM, NGROUP, NALL, NANGRE,
     4 NEXCSW, LIMASU, LOPSS2, LOPSS1, LOPSS8, LOPSS9, LOPSS10,
     5 LOPSS4, NSHARE
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'UMDECK.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'SYNCOM.FOR;/NOLIST'                                           
      REAL*8        TEXTA(101), TEXT3,TEXT4,TEXT5                       M42.3141
      REAL*8        TEXTM, TESM1, TESM2, TESM3, TESM4, TESM5            M42.3142
      REAL*8        TESM6, TESM7, TESM8, TEXT6, TEXT7                   M42.3143
      DATA TEXTM / 6HMORE  /                                            M42.3144
      DATA TESM1,TESM2,TESM3,TESM4,TESM5,TESM6,TESM7,TESM8,TESM9        M42.3145
     1 /6HSMDATA,6HTOLERA,6HPARAME,6HALL   ,6HNONE  ,6HEXTEND,          M42.3146
     2  6HFINISH,6H PART ,6HSHARE /                                     M42.3147
C  START READING SM TYPE -50 TO 59 DATA INPUT FORMAT **********         M42.3148
17040 INITUM = 1                                                        M42.3149
      NSMACH = NUMUM                                                    M42.3150
      N1 = NUMUM                                                        M42.3151
      JTYPE(N1) = 13                                                    M42.3152
      NCLD(N1) = 2                                                      M42.3153
      NCLQ(N1) = 2                                                      M42.3154
      JTMTAC(N1) = 0                                                    M42.3155
      NMEXC = 0                                                         M42.3156
      N5 = NCLTOT + 1                                                   M42.3157
      N6 = NCLTOT + 7                                                   M42.3158
      DO 17042 N10 = N5,N6                                              M42.3159
17042 FPAR(N10) = 0.0                                                   M42.3160
C  *************************** READ CLASS 1 SM TYPE-59 DATA             M42.3161
      DO 17070 N5 = 1,3                                                 M42.3162
      IF (N5 .NE. 1) CALL CIMAGE                                        M42.3163
      N6 = NCLTOT + N5                                                  M42.3164
      DECODE (80,17050, ABUFF(1)) N19, BUS1, D1, D2, D3                 M42.3165
17050 FORMAT(I2, A6, 2X, 3E10.6)                                        M42.3166
      IF (NOUTPR .EQ. 0)                                                M42.3167
     1 WRITE (KUNIT6, 17052)  NUMUM, N5                                 M42.3168
17052 FORMAT(5H+UM -, I3, 23H   SM-59 CLASS 1, CARD ,I1)                M42.3169
      IF (N5 .NE. 1) GO TO 17056                                        M42.3170
      NTYPSM = N19                                                      M42.3171
      IF (N19 .EQ. 59) GO TO 17054                                      M42.3172
      IF (N19 .EQ. 50) GO TO 17054                                      M42.3173
      IF (N19 .EQ. 52) GO TO 17054                                      M42.3174
      WRITE (LUNIT6,17053)                                              M42.3175
17053 FORMAT(/, 29H ERROR STOP. THE UM CODE DOES,                       M42.3176
     1          26H NOT ACCEPT DUAL MACHINES.)                          M42.3177
      GO TO 9600                                                                
17054 VOLTUM(N1) = D1                                                   M42.3179
      ANGLUM(N1) = D3                                                   M42.3180
      IF (D2 .EQ. 0.0) D2 = STATFR                                      M42.3181
      ROTMOM(N1) = TWOPI * D2                                           M42.3182
      STAT59 = D2                                                       M42.3183
C  CREATION OF 2 EXCITATION NODES :                                     M42.3184
      NTOT = NTOT + 1                                                   M42.3185
      KODE(NTOT) = 0                                                    M42.3186
      NODVO1(NCLTOT+4) = NTOT                                           M42.3187
      NODVO2(NCLTOT+4) = 1                                              M42.3188
      BUS(NTOT) = TRASH                                                 M42.3189
      NTOT = NTOT + 1                                                   M42.3190
      KODE(NTOT) = 0                                                    M42.3191
      NEXC = NTOT                                                       M42.3192
      BUS(NEXC) = TRASH                                                 M42.3193
      IF (NUMUM .GT. 5) GO TO 17056                                     M42.3194
      N3 = 41 + (NUMUM - 1)*4                                           M42.3195
      BUS(NTOT-1) = TEXTA(N3+1)                                         M42.3196
      BUS(NEXC) = TEXTA(N3+2)                                           M42.3197
17056 DO 17060 N10 = 1,NTOT                                             M42.3198
      IF (BUS1 .NE. BUS(N10)) GO TO 17060                               M42.3199
      NODVO1(N6) = N10                                                  M42.3200
      GO TO 17070                                                       M42.3201
17060 CONTINUE                                                          M42.3202
      WRITE(LUNIT6, 17062) BUS1                                         M42.3203
17062 FORMAT(/ , 40H ERROR STOP.   JUST-READ SM CLASS 1 CARD,           M42.3204
     1           25H BEARS ILLEGAL NODE NAME:, A6)                      M42.3205
      GO TO 9600                                                                
17070 CONTINUE                                                          M42.3207
      IF (IPRSUP .GE. 1) WRITE(LUNIT6,17072) N1                         M42.3208
17072 FORMAT(/, 10H *********,                                          M42.3209
     1          39H ELEMENTS INTERNALLY CREATED FOR UM NR.,I3,          M42.3210
     2           2H: ,12HNODE TO NODE,5X,3HIBR,6X,2HIT,2X,              M42.3211
     3           6HKSWTCH,2X,6HLSWTCH,2X,6HKCONST,9X,5HSFREQ,/)         M42.3212
C  SET VOLTAGE SOURCE FOR EXCITATION                                    M42.3213
      KCONST = KCONST + 1                                               M42.3214
      KCONEX = KCONST                                                   M42.3215
      NODFUM(N1) = KCONST                                               M42.3216
      IFORM(KCONST) = 14                                                M42.3217
      NODE(KCONST) = NEXC                                               M42.3218
      KODE(NEXC) = - NEXC                                               M42.3219
      SFREQ(KCONST) = EPSILN * 1.0D+3                                   M42.3220
      TSTART(KCONST) = - 1.0                                            M42.3221
      TSTOP(KCONST) = FLTINF                                            M42.3222
      IF (IPRSUP .GE. 1)                                                M42.3223
     1 WRITE(LUNIT6,17076) NODE(KCONST),KCONST,SFREQ(KCONST)            M42.3224
17076 FORMAT(10H *********,                                             M42.3225
     1       21H FIELD VOLTAGE SOURCE,23X,I4,44X,I4,E14.5)              M42.3226
C  SET SERIES AND PARALLEL RESISTANCE FOR EXCITATION                    M42.3227
      CALL IBRINC                                                       VAX.  51
      IBREXC = IBR                                                      M42.3229
      IT = IT + 1                                                       M42.3230
      KBUS(IBR) = NODVO1(NCLTOT+4)                                      M42.3231
      MBUS(IBR) = NEXC                                                  M42.3232
      LENGTH(IBR) = 1                                                   M42.3233
      NR(IBR) = - IT                                                    M42.3234
      TR(IT) = EPSILN                                                   M42.3235
      ITEXC = IT                                                        M42.3236
C  NOTE : TR(IT=ITEXC) WILL BE CHANGED LATER TO 0.5 * RF                M42.3237
      TX(IT) = 0.0                                                      M42.3238
      C(IT) = 0.0                                                       M42.3239
      IF (IPRSUP .GE. 1)                                                M42.3240
     1 WRITE(LUNIT6,17080) MBUS(IBR),KBUS(IBR),IBR,IT                   M42.3241
17080 FORMAT(10H *********,                                             M42.3242
     1        21H SERIES FIELD RESIST.,23X,I4,4X,I4,4X,I4,4X,I4)        M42.3243
      CALL IBRINC                                                       VAX.  52
      IT = IT + 1                                                       M42.3245
      KBUS(IBR) = NEXC                                                  M42.3246
      MBUS(IBR) = 1                                                     M42.3247
      LENGTH(IBR) = 1                                                   M42.3248
      NR(IBR) = - IT                                                    M42.3249
      TR(IT) = EPSILN                                                   M42.3250
      TX(IT) = 0.0                                                      M42.3251
      C(IT) = 0.0                                                       M42.3252
      IF (IPRSUP .GE. 1)                                                M42.3253
     1 WRITE(LUNIT6,17082) MBUS(IBR),KBUS(IBR),IBR,IT                   M42.3254
17082 FORMAT(10H *********,                                             M42.3255
     1        21H PARALL FIELD RESIST.,23X,I4,4X,I4,4X,I4,4X,I4)        M42.3256
C  SHORTING DAMPER AND EDDY CURRENT COILS                               M42.3257
      DO 17090 N5 = 5,7                                                 M42.3258
      NODVO1(NCLTOT+N5) = 1                                             M42.3259
      NODVO2(NCLTOT+N5) = 1                                             M42.3260
17090 CONTINUE                                                          M42.3261
C  *************************** READ CLASS 2 SM TYPE-59 DATA             M42.3262
C  NSTAN = 1 IS A FLAG FOR STANDARD MANUFACTURER DATA                   M42.3263
17100 NSTAN = 0                                                         M42.3264
      CALL CIMAGE                                                       M42.3265
      DECODE (80,17102,ABUFF(1)) TEXT3                                  M42.3266
17102 FORMAT(A6)                                                        M42.3267
      IF (TEXT3 .NE. TESM2) GO TO 17106                                 M42.3268
      IF (NOUTPR .EQ. 0)                                                M42.3269
     1 WRITE (KUNIT6, 17104)  NUMUM                                     M42.3270
17104 FORMAT(5H+UM -, I3, 32H   SM-59 CLASS 2, TOLERANCE CARD)          M42.3271
      CALL CIMAGE                                                       M42.3272
17106 N19 = 1                                                           M42.3273
      DECODE (80,17107,ABUFF(1)) TEXT3,FMUM                             M42.3274
17107 FORMAT(A6, 18X, E8.0)                                             M42.3275
      IF (TEXT3 .NE. TESM3) GO TO 17200                                 M42.3276
      IF (NOUTPR .EQ. 0)                                                M42.3277
     1 WRITE (KUNIT6, 17108)  NUMUM                                     M42.3278
17108 FORMAT(5H+UM -, I3, 33H   SM-59 CLASS 2, PAR FIT REQUEST)         M42.3279
      NSTAN = 1                                                         M42.3280
      CALL CIMAGE                                                       M42.3281
C  *************************** READ CLASS 3 SM TYPE-59 DATA             M42.3282
17200 N2 = 0                                                            M42.3283
      DECODE (80,17210,ABUFF(1)) NUMASU, NMGEN, NMEXC, N20, D17,        M42.3284
     1 RMVAUM, RKVUM, AGLDUM, S1UM, S2UM                                M42.3285
17210 FORMAT(3I2, I4, F6.0, 14X, 5E10.6)                                M42.3286
      IF (NUMASU .EQ. 0) NUMASU = D17                                   M42.3287
      IF (NOUTPR .EQ. 0)                                                M42.3288
     1 WRITE (KUNIT6, 17212)  NUMUM                                     M42.3289
17212 FORMAT (5H+UM -, I3, 17H   SM-59 CLASS 3,,                        M42.3290
     1                     19H GENERAL PARAMETERS)                      M42.3291
C   ZLSBUM IS THE STATOR BASE INDUCTANCE, IF MULTIPLIED WITH            M42.3292
C      ROTMOM(N1), IT BECOMES THE STATOR BASE IMPEDANCE.                M42.3293
      RMVAUM = RMVAUM * 1.0D+6                                          M42.3294
      RKVUM = RKVUM * 1.0D+3                                            M42.3295
      ZLSBUM = RKVUM * RKVUM                                            M42.3296
      ZLSBUM = ZLSBUM/(ROTMOM(N1) * RMVAUM)                             M42.3297
      JCDSAT(N1) = 0                                                    M42.3298
      IF (AGLDUM .LT. 0.0) JCDSAT(N1) = 1                               M42.3299
      IF (AGLDUM .LT. 0.0) AGLDUM = - AGLDUM                            M42.3300
17214 NPPAIR(N1) = N20/2                                                M42.3301
      N5 = 2                                                            M42.3302
      N6 = NPPAIR(N1) * N5                                              M42.3303
      IF (N6 .EQ. N20) GO TO 17218                                      M42.3304
      WRITE (LUNIT6,17216) N1                                           M42.3305
17216 FORMAT( /, 41H ERROR STOP. ERRONEOUS DATA INPUT BECAUSE,          M42.3306
     1           35H AN UNEVEN NUMBER OF POLES HAS BEEN,                M42.3307
     2           26H SPECIFIED FOR MACH NUMBER, I4, 1H.)                M42.3308
      GO TO 9600                                                                
17218 CALL CIMAGE                                                       M42.3310
      DECODE (80,17222,ABUFF(1)) TEXT3, TEXT4, TEXT5,                   M42.3311
     1 AGLQUM, S1QUM, S2QUM                                             M42.3312
17222 FORMAT(3A6, 32X, 3E10.6)                                          M42.3313
      IF (TEXT3 .NE. BLANK) GO TO 17224                                 M42.3314
      IF (TEXT4 .NE. BLANK) GO TO 17224                                 M42.3315
      IF (TEXT5 .NE. BLANK) GO TO 17224                                 M42.3316
      GO TO 17226                                                       M42.3317
17224 AGLQUM = 0.0                                                      M42.3318
      S1QUM = 0.0                                                       M42.3319
      S2QUM = 0.0                                                       M42.3320
      GO TO 17230                                                       M42.3321
17226 IF (NOUTPR .EQ. 0)                                                M42.3322
     1 WRITE (KUNIT6, 17228)  NUMUM                                     M42.3323
17228 FORMAT(5H+UM -,I3, 17H   SM-59 CLASS 3,,                          M42.3324
     1                   23H Q-AXIS SATURATION CARD)                    M42.3325
      CALL CIMAGE                                                       M42.3326
17230 JCQSAT(N1) = 0                                                    M42.3327
      D1 = S1QUM + S2QUM                                                M42.3328
      IF (D1 .NE. 0.0) JCQSAT(N1) = 1                                   M42.3329
      IF (AGLQUM .GE. 0.0) GO TO 17240                                  M42.3330
      JCQSAT(N1) = 1                                                    M42.3331
      AGLQUM = 1.6 * AGLDUM                                             M42.3332
      S1QUM = 1.5 * S1UM                                                M42.3333
      S2QUM = 1.5 * S2UM                                                M42.3334
C  READING STANDARD MANUFACTURER DATA :                                 M42.3335
17240 IF (NTYPSM .EQ. 52) GO TO 17400                                   M42.3336
      IF (NTYPSM .EQ. 53) GO TO 17400                                   M42.3337
      IF (NTYPSM .EQ. 50) GO TO 17241                                   M42.3338
      IF (NTYPSM .EQ. 51) GO TO 17241                                   M42.3339
      IF (NSTAN .NE. 1) GO TO 17400                                     M42.3340
17241 N2 = 0                                                            M42.3341
      DECODE (80,17242,ABUFF(1)) RAUM, XLUM, XDUM, XQUM, XDPUM,         M42.3342
     1 XQPUM, XDPPUM, XQPPUM                                            M42.3343
17242 FORMAT(8E10.6)                                                    M42.3344
      N2 = N2 + 1                                                       M42.3345
      IF (NOUTPR .EQ. 0)                                                M42.3346
     1 WRITE (KUNIT6, 17244)  NUMUM, N2                                 M42.3347
17244 FORMAT(5H+UM -,I3,17H   SM-59 CLASS 3,,                           M42.3348
     1                  18H MANUFACTURE CARD ,I1)                       M42.3349
      CALL CIMAGE                                                       M42.3350
      DECODE (80,17246,ABUFF(1)) TDPUM, TQPUM, TDPPUM, TQPPUM,          M42.3351
     1 X0UM, RNUM, XNUM, NETRUM                                         M42.3352
17246 FORMAT(7E10.6, I10)                                               M42.3353
      N2 = N2 + 1                                                       M42.3354
      IF (NOUTPR .EQ. 0)                                                M42.3355
     1 WRITE (KUNIT6, 17244)  NUMUM, N2                                 M42.3356
C  CONVERSION OF PU SYSTEM TO STATOR REFERED SI SYSTEM :                M42.3357
      RAUM = RAUM * ZLSBUM * ROTMOM(N1)                                 M42.3358
      XLUM = XLUM * ZLSBUM                                              M42.3359
      XDUM = XDUM * ZLSBUM                                              M42.3360
      XQUM = XQUM * ZLSBUM                                              M42.3361
      XDPUM = XDPUM * ZLSBUM                                            M42.3362
      XQPUM = XQPUM * ZLSBUM                                            M42.3363
      XDPPUM = XDPPUM * ZLSBUM                                          M42.3364
      XQPPUM = XQPPUM * ZLSBUM                                          M42.3365
      X0UM = X0UM * ZLSBUM                                              M42.3366
      RNUM = RNUM * ZLSBUM * ROTMOM(N1)                                 M42.3367
      XNUM = XNUM * ZLSBUM                                              M42.3368
C  ESTABLISHING STATOR AND MACH TABLE VARIABLES                         M42.3369
      IF (RAUM .EQ. 0.0) RAUM = EPSILN                                  M42.3370
      DO 17260 N5 = 1,3                                                 M42.3371
      GPAR(NCLTOT+N5) = RAUM                                            M42.3372
      IF (N5 .EQ. 1) GO TO 17260                                        M42.3373
      REACL(NCLTOT+N5) = XLUM                                           M42.3374
17260 CONTINUE                                                          M42.3375
      REACL(NCLTOT+1) = X0UM                                            M42.3376
      REAMDU(N1) = XDUM - XLUM                                          M42.3377
      REAMQU(N1) = XQUM - XLUM                                          M42.3378
      IF (REAMQU(N1) .GT. 0.0) GO TO 17262                              M42.3379
      WRITE(LUNIT6,6198) N1                                             M42.3380
 6198 FORMAT( /, 41H ERROR STOP. INCORRECT UM DATA INPUT. THE,          M42.3381
     1           42H RESULT IS THAT THE Q-AXIS MAIN INDUCTANCE          M42.3382
     2  ,/,7X,   39H IS EITHER ZERO OR NEGATIVE FOR MACHINE,            M42.3383
     3            7H NUMBER, I4, 1H.)                                   M42.3384
      GO TO 9600                                                                
17262 IF (XQUM .NE. XQPUM) GO TO 17290                                  M42.3386
      IF (XQUM .NE. XQPPUM) GO TO 17280                                 M42.3387
      GPAR(NCLTOT+6) = 0.0                                              M42.3388
      GPAR(NCLTOT+7) = 0.0                                              M42.3389
      REACL(NCLTOT+6) = 1.0                                             M42.3390
      REACL(NCLTOT+7) = 1.0                                             M42.3391
      GO TO 17290                                                       M42.3392
17280 IF (FMUM .NE. 1.0) GO TO 17285                                    M42.3393
      GPAR(NCLTOT+6) = 0.0                                              M42.3394
      REACL(NCLTOT+6) = 1.0                                             M42.3395
17285 IF (FMUM .EQ. 0.0) FMUM = 0.95                                    M42.3396
      IF (FMUM .LT. 1.0) XQPUM = FMUM * XQUM                            M42.3397
17290 IF (XDUM .NE. XDPUM) GO TO 17310                                  M42.3398
      IF (XDUM .NE. XDPPUM) GO TO 17300                                 M42.3399
      GPAR(NCLTOT+4) = 0.0                                              M42.3400
      GPAR(NCLTOT+5) = 0.0                                              M42.3401
      REACL(NCLTOT+4) = 1.0                                             M42.3402
      REACL(NCLTOT+5) = 1.0                                             M42.3403
      GO TO 17310                                                       M42.3404
17300 IF (FMUM .NE. 1.0) GO TO 17305                                    M42.3405
      GPAR(NCLTOT+4) = 0.0                                              M42.3406
      REACL(NCLTOT+4) = 1.0                                             M42.3407
17305 IF (FMUM .EQ. 0.0) FMUM = 0.3                                     M42.3408
      IF (FMUM .LT. 1.0) XDPUM = FMUM * XDUM                            M42.3409
C  ESTABLISHING ROTOR VARIABLES :                                       M42.3410
17310 D1 = REAMDU(N1) * REAMDU(N1)                                      M42.3411
      IF (REACL(NCLTOT+4) .NE. 1.0) GO TO 17320                         M42.3412
      IF (REACL(NCLTOT+5) .EQ. 1.0) GO TO 17330                         M42.3413
      D10 = D1/(XDUM - XDPPUM)                                          M42.3414
      REACL(NCLTOT+5) = D10 - REAMDU(N1)                                M42.3415
      D10 = REAMDU(N1) + REACL(NCLTOT+5)                                M42.3416
      GPAR(NCLTOT+5) = D10/TDPPUM                                       M42.3417
      GO TO 17330                                                       M42.3418
17320 D10 = D1/(XDUM - XDPUM)                                           M42.3419
      REACL(NCLTOT+4) = D10 - REAMDU(N1)                                M42.3420
      D10 = REAMDU(N1) + REACL(NCLTOT+4)                                M42.3421
      GPAR(NCLTOT+4) = D10/TDPUM                                        M42.3422
      IF (REACL(NCLTOT+5) .EQ. 1.0) GO TO 17330                         M42.3423
      D11 = XDUM - XDPPUM + D10 - 2.0 * REAMDU(N1)                      M42.3424
      D11 = D11 * D1                                                    M42.3425
      D12 = (XDUM - XDPPUM) * D10 - D1                                  M42.3426
      REACL(NCLTOT+5) = D11/D12 - REAMDU(N1)                            M42.3427
      D11 = REAMDU(N1) + REACL(NCLTOT+5)                                M42.3428
      D12 = D1/D10                                                      M42.3429
      GPAR(NCLTOT+5) = (D11 - D12)/TDPPUM                               M42.3430
17330 D1 = REAMQU(N1) * REAMQU(N1)                                      M42.3431
      IF (REACL(NCLTOT+6) .NE. 1.0) GO TO 17340                         M42.3432
      IF (REACL(NCLTOT+7) .EQ. 1.0) GO TO 17500                         M42.3433
      D10 = D1/(XQUM - XQPPUM)                                          M42.3434
      REACL(NCLTOT+7) = D10 - REAMQU(N1)                                M42.3435
      D10 = REAMQU(N1) + REACL(NCLTOT+7)                                M42.3436
      GPAR(NCLTOT+7) = D10/TQPPUM                                       M42.3437
      GO TO 17500                                                       M42.3438
17340 D10 = D1/(XQUM - XQPUM)                                           M42.3439
      REACL(NCLTOT+6) = D10 - REAMQU(N1)                                M42.3440
      D10 = REAMQU(N1) + REACL(NCLTOT+6)                                M42.3441
      GPAR(NCLTOT+6) = D10/TQPUM                                        M42.3442
      IF (REACL(NCLTOT+7) .EQ. 1.0) GO TO 17500                         M42.3443
      D11 = XQUM - XQPPUM + D10 - 2.0 * REAMQU(N1)                      M42.3444
      D11 = D11 * D1                                                    M42.3445
      D12 = (XQUM - XQPPUM) * D10 - D1                                  M42.3446
      REACL(NCLTOT+7) = D11/D12 - REAMQU(N1)                            M42.3447
      D11 = REAMQU(N1) + REACL(NCLTOT+7)                                M42.3448
      D12 = D1/D10                                                      M42.3449
      GPAR(NCLTOT+7) = (D11 - D12)/TQPPUM                               M42.3450
      GO TO 17500                                                       M42.3451
C  PROCESSING STANDARD MANUFACTURER DATA COMPLETED.                     M42.3452
C  READING OF INDUCTANCE/RESISTANCE DATA INPUT :                        M42.3453
17400 N2 = 0                                                            M42.3454
      DECODE (80,17410,ABUFF(1)) XFUM, XDFUM, XFKDUM, XDUM,             M42.3455
     1 XDKDUM, XKDUM                                                    M42.3456
17410 FORMAT(6E10.6)                                                    M42.3457
      N2 = N2 + 1                                                       M42.3458
      IF (NOUTPR .EQ. 0)                                                M42.3459
     1 WRITE (KUNIT6, 17412)  NUMUM, N2                                 M42.3460
17412 FORMAT(5H+UM -,I3,17H   SM-59 CLASS 3,,                           M42.3461
     1                  20H R AND L PARAM CARD ,I1)                     M42.3462
      CALL CIMAGE                                                       M42.3463
      DECODE (80,17420,ABUFF(1)) XKQUM, XQKQUM, XGKQUM, XQUM,           M42.3464
     1 XQGUM,XGUM,NETRUM                                                M42.3465
17420 FORMAT(6E10.6, 10X, I10)                                          M42.3466
      N2 = N2 + 1                                                       M42.3467
      IF (NOUTPR .EQ. 0)                                                M42.3468
     1 WRITE (KUNIT6, 17412)  NUMUM, N2                                 M42.3469
      CALL CIMAGE                                                       M42.3470
      DECODE(80,17430,ABUFF(1)) REACL(NCLTOT+1),GPAR(NCLTOT+1),         M42.3471
     1 GPAR(NCLTOT+4), GPAR(NCLTOT+5), GPAR(NCLTOT+6),                  M42.3472
     2 GPAR(NCLTOT+7), RNUM, XNUM                                       M42.3473
17430 FORMAT(8E10.6)                                                    M42.3474
      N2 = N2 + 1                                                       M42.3475
      IF (NOUTPR .EQ. 0)                                                M42.3476
     1 WRITE (KUNIT6, 17412)  NUMUM, N2                                 M42.3477
C  CONVERSION PU SYSTEM INTO STATOR REFERED SI SYSTEM AND UM            M42.3478
C    PARAMETER INPUT                                                    M42.3479
      RNUM = RNUM * ZLSBUM * ROTMOM(N1)                                 M42.3480
      XNUM = XNUM * ZLSBUM                                              M42.3481
      IF (GPAR(NCLTOT+1) .EQ. 0.0) GPAR(NCLTOT+1) = EPSILN              M42.3482
      GPAR(NCLTOT+2) = GPAR(NCLTOT+1)                                   M42.3483
      GPAR(NCLTOT+3) = GPAR(NCLTOT+1)                                   M42.3484
      D1 = ZLSBUM * ROTMOM(N1)                                          M42.3485
      DO 17432 N5 = 1,7                                                 M42.3486
17432 GPAR(NCLTOT+N5) = GPAR(NCLTOT+N5) * D1                            M42.3487
      REAMDU(N1) = XDFUM                                                M42.3488
      REAMQU(N1) = XQKQUM                                               M42.3489
      REACL(NCLTOT+1) = REACL(NCLTOT+1) * ZLSBUM                        M42.3490
      REACL(NCLTOT+2) = (XDUM - REAMDU(N1)) * ZLSBUM                    M42.3491
      REACL(NCLTOT+3) = (XQUM - REAMQU(N1)) * ZLSBUM                    M42.3492
      REACL(NCLTOT+4) = (XFUM - REAMDU(N1)) * ZLSBUM                    M42.3493
      REACL(NCLTOT+5) = (XKDUM - REAMDU(N1)) * ZLSBUM                   M42.3494
      REACL(NCLTOT+6) = (XKQUM - REAMQU(N1)) * ZLSBUM                   M42.3495
      REACL(NCLTOT+7) = (XGUM - REAMQU(N1)) * ZLSBUM                    M42.3496
      REAMDU(N1) = REAMDU(N1) * ZLSBUM                                  M42.3497
      REAMQU(N1) = REAMQU(N1) * ZLSBUM                                  M42.3498
C  REDUCTION FACTOR 1/KF = IFB/ISB , STORED IN DCOEF(N1) :              M42.3499
17500 DCOEF(N1) = REAMDU(N1)*AGLDUM * ROTMOM(N1)/RKVUM                  M42.3500
C  START PROCESSING SATURATION :                                        M42.3501
      FLXDS(N1) = 0.0                                                   M42.3502
      FLXQS(N1) = 0.0                                                   M42.3503
      REAMDS(N1) = 0.0                                                  M42.3504
      REAMQS(N1) = 0.0                                                  M42.3505
      FLXDR(N1) = 0.0                                                   M42.3506
      FLXQR(N1) = 0.0                                                   M42.3507
      IF (JCDSAT(N1) .EQ. 0) JCQSAT(N1) = 0                             M42.3508
      IF (JCDSAT(N1) .EQ. 0) GO TO 17510                                M42.3509
      REAMDS(N1) = 0.2*REAMDU(N1)*AGLDUM/(S2UM-S1UM)                    M42.3510
      D1 = REAMDS(N1) - EPSILN                                          M42.3511
      IF (D1 .LE. REAMDU(N1)) GO TO 17504                               M42.3512
      WRITE(LUNIT6,17502)                                               M42.3513
17502 FORMAT(/,  38H ERROR STOP. YOU HAVE CHOSEN INCORRECT,             M42.3514
     1           42H D - AXIS SATURATION PARAMETERS S1 AND S2.)         M42.3515
      WRITE (LUNIT6,17503) N1                                           M42.3516
17503 FORMAT(/,45H THE RESULT IS A SATURATED INDUCTANCE GREATER,        M42.3517
     1           38H THAN THE UNSATURATED ONE. THE MACHINE,             M42.3518
     2           24H CONCERNED IS UM NUMBER , I4, 1H.                   M42.3519
     3  ,/,      41H PLEASE TAKE REALISTICALLY EITHER A LOWER,          M42.3520
     4           39H VALUE FOR S1 OR A HIGHER VALUE FOR S2.)            M42.3521
      GO TO 9600                                                                
17504 FLXDS(N1) = (S2UM - 1.2*S1UM) * RKVUM/ROTMOM(N1)                  M42.3523
      D2 = S2UM - S1UM - 0.2*AGLDUM                                     M42.3524
      IF (D2 .NE. 0.0) GO TO 17505                                      M42.3525
      JCDSAT(N1) = 0.0                                                  M42.3526
      JCQSAT(N1) = 0.0                                                  M42.3527
      GO TO 17510                                                       M42.3528
17505 FLXDS(N1) = FLXDS(N1) /D2                                         M42.3529
      D10 = 1.0 - REAMDS(N1)/REAMDU(N1)                                 M42.3530
      D10 = D10 * FLXDS(N1)                                                     
      IF (D10 .GE. 0.0) GO TO 97510                                     M42.3532
      WRITE (LUNIT6,97506)                                              M42.3533
97506 FORMAT(/,39H ERROR STOP.  YOU HAVE CHOSEN INCORRECT,              M42.3534
     1         40H D-AXIS SATURATION PARAMETERS S1 AND S2.)             M42.3535
      WRITE (LUNIT6,97507) N1                                           M42.3536
97507 FORMAT(40H THE RESULT IS AN UNREALISTIC SATURATION,               M42.3537
     1       47H CHARACTERISTIC WITH THE SATURATED LINE-SEGMENT,        M42.3538
     2       46H NOT INTERSECTING THE UNSATURATED LINE-SEGMENT          M42.3539
     3  ,/,  45H IN THE FIRST QUADRANT. THE MACHINE CONCERNED,          M42.3540
     4       13H IS UM NUMBER,I4,1H.)                                   M42.3541
      GO TO 9600                                                                
97510 IF (JCQSAT(N1) .EQ. 0) GO TO 17510                                M42.3543
      REAMQS(N1) = 0.2*REAMQU(N1)*AGLQUM/(S2QUM-S1QUM)                  M42.3544
      D1 = REAMQS(N1) - EPSILN                                          M42.3545
      IF (D1 .LE. REAMQU(N1)) GO TO 17508                               M42.3546
      WRITE(LUNIT6,17506)                                               M42.3547
17506 FORMAT(/,  38H ERROR STOP. YOU HAVE CHOSEN INCORRECT,             M42.3548
     1           42H Q - AXIS SATURATION PARAMETERS S1 AND S2.)         M42.3549
      WRITE(LUNIT6,17503) N1                                            M42.3550
      GO TO 9600                                                                
17508 FLXQS(N1) = (S2QUM-1.2*S1QUM) * RKVUM/ROTMOM(N1)                  M42.3552
      D2 = S2QUM - S1QUM - 0.2*AGLQUM                                   M42.3553
      IF (D2 .NE. 0.0) GO TO 17509                                      M42.3554
      JCQSAT(N1) = 0.0                                                  M42.3555
      GO TO 17510                                                       M42.3556
17509 FLXQS(N1) = FLXQS(N1)/D2                                          M42.3557
      D10 = 1.0 - REAMQS(N1)/REAMQU(N1)                                 M42.3558
      D10 = D10 * FLXQS(N1)                                                     
      IF (D10 .GE. 0.0) GO TO 17510                                     M42.3560
      WRITE(LUNIT6,97520)                                               M42.3561
97520 FORMAT(/,39H ERROR STOP.  YOU HAVE CHOSEN INCORRECT,              M42.3562
     1         40H Q-AXIS SATURATION PARAMATERS S1 AND S2.)             M42.3563
      WRITE(LUNIT6,97507) N1                                            M42.3564
      GO TO 9600                                                                
C  START PROCESSING NEUTRAL ELEMENT VALUES :                            M42.3566
17510 D2 = 1.0D+2                                                       M42.3567
      IF (NETRUM .EQ. 1) RNUM = D2                                      M42.3568
      D1 = RNUM + XNUM * 1.0D+3                                         M42.3569
      IF (D1 .GT. D2) RNUM = D2                                         M42.3570
      IF (D1 .GT. D2) XNUM = 0.0                                        M42.3571
      N7 = 1                                                            M42.3572
C  CREATION OF NEUTRAL NODE                                             M42.3573
      DO 17530 N5 = 1,3                                                 M42.3574
      N6 = NCLTOT + N5                                                  M42.3575
      IF (D1 .EQ. 0.0) GO TO 17520                                      M42.3576
      IF (N5 .GT. 1) GO TO 17520                                        M42.3577
      NTOT = NTOT + 1                                                   M42.3578
      KODE(NTOT) = 0                                                    M42.3579
      N7 = NTOT                                                         M42.3580
      BUS(NTOT) = TRASH                                                 M42.3581
      IF (NUMUM .GT. 5) GO TO 17520                                     M42.3582
      BUS(NTOT) = TEXTA(NUMUM+60)                                       M42.3583
17520 NODVO2(N6) = N7                                                   M42.3584
17530 CONTINUE                                                          M42.3585
C  CREATE HIGH RESISTANCES IN PARALLEL OVER COILS IF                    M42.3586
      IF (LOOPSS(8) .NE. 0) GO TO 17536                                 M42.3587
      DO 17532 N5 = 1,3                                                 M42.3588
      CALL IBRINC                                                       VAX.  53
      IT = IT + 1                                                       M42.3590
      KBUS(IBR) = NODVO1(NCLTOT+N5)                                     M42.3591
      MBUS(IBR) = NODVO2(NCLTOT+N5)                                     M42.3592
      LENGTH(IBR) = 1                                                   M42.3593
      NR(IBR) = - IT                                                    M42.3594
      TR(IT) = 1.0D+8                                                   M42.3595
      TX(IT) = 0.0                                                      M42.3596
      C(IT) = 0.0                                                       M42.3597
      IF (IPRSUP .GE. 1)                                                M42.3598
     1 WRITE (LUNIT6,17531) MBUS(IBR),KBUS(IBR),IBR,IT                  M42.3599
17531 FORMAT(10H *********,                                             M42.3600
     1        19H HIGH POWER RESIST.,25X,I4,4X,I4,4X,I4,4X,I4)          M42.3601
17532 CONTINUE                                                          M42.3602
C  INSERTION OF NEUTRAL IMPEDANCE                                       M42.3603
17536 IF (D1 .EQ. 0.0) GO TO 17540                                      M42.3604
      CALL IBRINC                                                       VAX.  54
      IT = IT + 1                                                       M42.3606
      KBUS(IBR) = NODVO2(NCLTOT+1)                                      M42.3607
      MBUS(IBR) = 1                                                     M42.3608
      LENGTH(IBR) = 1                                                   M42.3609
      NR(IBR) = - IT                                                    M42.3610
      TR(IT) = RNUM                                                     M42.3611
      IF (XOPT .EQ. 0.0) TX(IT) = XNUM * 1.0D+3                         M42.3612
      IF (XOPT .NE. 0.0) TX(IT) = XNUM * TWOPI * XOPT                   M42.3613
      C(IT) = 0.0                                                       M42.3614
      IF (IPRSUP .GE. 1)                                                M42.3615
     1 WRITE(LUNIT6,17539) MBUS(IBR),KBUS(IBR),IBR,IT                   M42.3616
17539 FORMAT(10H *********,                                             M42.3617
     1        18H NEUTRAL IMPEDANCE,26X,I4,4X,I4,4X,I4,4X,I4)           M42.3618
C  SET 0.5*RF FOR EXTERNAL EXCITER RESISTANCE:                          M42.3619
C    (INTERNAL FIELD RESISTANCE ADJUSTED AT 17958)                      M42.3620
17540 TR(ITEXC) = 0.5*GPAR(NCLTOT+4)/(DCOEF(N1)*DCOEF(N1))              M42.3621
C  *************************** READ CLASS 4 SM TYPE-59 DATA             M42.3622
      D10 = 1.0D+6                                                      M42.3623
      NSMTPR = NSMTAC                                                   M42.3624
      NRSYN = 0                                                         M42.3625
      NTORQ = 0                                                         M42.3626
C   NTOTST + K = EMTP NODE NR OF MASS NR. K                             M42.3627
C   NMGEN NOW BECOMES EMTP NODE NR OF GENERATOR MASS                    M42.3628
C   NMEXC NOW BECOMES EMTP NODE NR OF EXCITER MASS                      M42.3629
      JTMTAC(N1) = 0                                                    M42.3630
      CALL CIMAGE                                                       M42.3631
      DECODE(80,17550,ABUFF(1)) TEXT3                                   M42.3632
17550 FORMAT(A6)                                                        M42.3633
      IF (TEXT3 .NE. TESM9) GO TO 17570                                 M42.3634
      DECODE(80,17552,ABUFF(1)) N3,N4                                   M42.3635
17552 FORMAT(6X,2I6)                                                    M42.3636
      IF (NOUTPR .EQ. 0) WRITE(KUNIT6,17553) NUMUM                      M42.3637
17553 FORMAT(5H+UM -,I3,33H   SM-59 CLASS 4, SHARE MECH NETW)           M42.3638
      IF (N1 .NE. N3 .AND. N1 .NE. N4) GO TO 17560                      M42.3639
      WRITE (LUNIT6,15827) N1                                           M42.3640
15827 FORMAT(/,21H ERROR STOP. THIS UM-,I3,15H IS SUPPOSED TO,          M42.3641
     1         45H SHARE ITS MECH NETWORK WITH OTHER UM'S. WHAT,        M42.3642
     2         41H SHOULD BE SPECIFIED ON THIS CARD ARE THE             M42.3643
     3  ,/,    36H NUMBERS OF THESE OTHER UM'S WITHOUT,                 M42.3644
     4         40H INCLUDING THE NUMBER OF THE UM WHICH IS,             M42.3645
     5         27H BEING PROCESSED RIGHT NOW.)                          M42.3646
      GO TO 9600                                                                
17560 N6 = N1                                                                   
      IF (N3 .NE. 0 .AND. N3 .LT. N6) N6 = N3                           M42.3649
      IF (N4 .NE. 0 .AND. N4 .LT. N6) N6 = N4                           M42.3650
C  ONLY THE LOWEST NUMBERED UM OF THE MACHINES SHARING                  M42.3651
C   A COMMON MECH NETWORK ARE SPECIFIED WITH THE MECH NETWORK           M42.3652
C   STRUCTURE (THROUGH THE MASS CARDS OF CLASS 4). THE FLAG             M42.3653
C   JTMTAC(N1)=-999999 INDICATES THAT UM NUMBER N1 IS NOT THE           M42.3654
C   THIS LOWEST NUMBERED UM.                                            M42.3655
      IF (N1 .EQ. N6) JTMTAC(N1) = - NTOT                               M42.3656
      IF (N1 .NE. N6) JTMTAC(N1) = - 999999                             M42.3657
      NTOTST = - JTMTAC(N6)                                             M42.3658
      GO TO 17571                                                       M42.3659
17570 NTOTST = NTOT                                                     M42.3660
17571 NMGEN = NTOTST + NMGEN                                            M42.3661
      IF (NMEXC .EQ. 0) GO TO 17575                                     M42.3662
      NMEXC = NTOTST + NMEXC                                            M42.3663
C. STORING IN FPAR AND CREATE CURRENT SOURCE FOR "TQEXC" :              M42.3664
      KCONST = KCONST + 1                                               M42.3665
      IFORM(KCONST) = 14                                                M42.3666
      NODE(KCONST) = - NMEXC                                            M42.3667
      KODE(NMEXC) = 0                                                   M42.3668
      CREST(KCONST) = 0.0                                               M42.3669
      TSTART(KCONST) = - 1.0                                            M42.3670
      TSTOP(KCONST) = FLTINF                                            M42.3671
      SFREQ(KCONST) = 0.00001                                           M42.3672
      IF (IPRSUP .GE. 1)                                                M42.3673
     1 WRITE(LUNIT6,17572) NODE(KCONST),KCONST,SFREQ(KCONST)            M42.3674
17572 FORMAT(10H *********,                                             M42.3675
     1       22H EXCITER TORQUE SOURCE,22X,I4,44X,I4,E14.5)             M42.3676
      FPAR(NCLTOT+4) = - KCONST                                                 
      FPAR(NCLTOT+5) = NMEXC                                                    
      FPAR(NCLTOT+6) = NEXC                                                     
17575 NODOM(N1) = NMGEN                                                 M42.3680
C  SET ALL MECHANICAL NODES                                             M42.3681
      IF (JTMTAC(N1) .EQ. -999999) GO TO 17700                          M42.3682
      NTOT = NTOT + 2 * NUMASU                                          M42.3683
      N6 = NTOTST + 1                                                   M42.3684
      DO 17577 N5 = N6,NTOT                                             M42.3685
17577 KODE(N5) = 0                                                      M42.3686
C  NSMTPR IS TO STORE THE VALUE OF NSMTAC OF THE PREVIOUS MACH.         M42.3687
C  NSMTAC IS THE TOTAL NR OF VARIABLES TO BE PASSED TO TACS.            M42.3688
C  NTORQ IS THE NUMBER OF MASSES WITH DISTRF .NE. 0.0 , THUS            M42.3689
C    FOR THE MASSES WITH AN EXTERNALLY APPLIED TORQUE.                  M42.3690
C  STORAGE APPROACH IN UMOUTP FOR EACH SM :                             M42.3691
C    THE FIRST NUMASU CELLS BEHIND NSMTPR IS FOR DISTRF.                M42.3692
C    THE SECOND NUMASU CELLS FOR L12 (AND OF COURSE L23,ETC).           M42.3693
C    THE THIRD NUMASU CELLS FOR L12/R12 .                               M42.3694
C    THEN THE REST FOR QUANTITIES REFLECTING TACS TRANSFER.             M42.3695
C    AFTER COMPLETION OF READING ALL SM DATA, THE 3*NUMASU              M42.3696
C      CELLS BEHIND NSMTPR ARE DESTROYED AND THE QUANTITIES             M42.3697
C      FOR TACS TRANSFER ARE SHIFTED TO BEHIND NSMTPR.                  M42.3698
C  STORAGE APPROACH FOR EACH VARIABLE TO BE TRANSFERED :                M42.3699
C   * FOR VOLTAGES (SPEEDS) :                                           M42.3700
C          FIRST CELL = CODE NR PROVIDED WITH NEG SIGN                  M42.3701
C          SECOND CELL = NODE NR                                        M42.3702
C          THIRD CELL = TACS TABLE NR (NTOTAC)                          M42.3703
C   * FOR CURRENTS (TORQUES) :                                          M42.3704
C        SAME AS VOLTAGES, EXCEPT THIRD CELL = SWITCH TABLE NR.         M42.3705
C   * FOR MASS ANGLES :                                                 M42.3706
C          FIRST, SECOND AND THIRD CELL SAME AS VOLTAGES                M42.3707
C          FOURTH CELL = ANGLE HISTORY, ANGLE PART                      M42.3708
C          FIFTH CELL = ANGLE HISTORY, OMEGA*DELTAT/2 PART              M42.3709
17580 DO 17660 N5 = 1,NUMASU                                            M42.3710
      IF (N5 .EQ. 1 .AND. JTMTAC(N1) .EQ. 0) GO TO 17582                M42.3711
      CALL CIMAGE                                                       M42.3712
17582 N17 = 0                                                                   
      DECODE (80,17600,ABUFF(1)) MLUM, N17, DISTRF, HJUM,               M42.3714
     1 DSYNUM, DMUTUM, SPRING, DABSUM, BUS1                             M42.3715
17600 FORMAT(I2,I6,2X,6E10.6,A6)                                        M42.3716
      IF (MLUM .EQ. 0) MLUM = N17                                       M42.3717
      IF (NOUTPR .EQ. 0)                                                M42.3718
     1 WRITE (KUNIT6, 17602)  NUMUM, MLUM                               M42.3719
17602 FORMAT(5H+UM -,I3, 26H   SM-59 CLASS 4, MASS NR.,I6)              M42.3720
      IF (MLUM .LE. NUMASU) GO TO 17604                                 M42.3721
      WRITE(LUNIT6,17603) MLUM,NUMASU                                   M42.3722
17603 FORMAT(/,45H ERROR STOP. THE LAST CARD INDICATES THE DATA,        M42.3723
     1         15H OF MASS NUMBER,I6,24H, AND YET IN CLASS-3 OF         M42.3724
     2   ,/,   40H OF SM DATA CARDS YOU HAVE SPECIFIED THE,             M42.3725
     3         43H TOTAL NUMBER OF MASSES "NUMAS" TO BE EQUAL,          M42.3726
     4          3H TO,I6,1H.                                            M42.3727
     5   ,/,   29H PLEASE REMOVE THIS CONFLICT.)                        M42.3728
      GO TO 9600                                                                
C  OUTPUT NAMES OF MECHANICAL NODES :                                   M42.3730
17604 BUS(NTOTST+MLUM) = BUS1                                           M42.3731
C  CREATING MASS ELEMENT                                                M42.3732
      CALL IBRINC                                                       VAX.  55
      IT = IT + 1                                                       M42.3734
      KBUS(IBR) = NTOTST + MLUM                                         M42.3735
      MBUS(IBR) = 1                                                     M42.3736
      LENGTH(IBR) = 1                                                   M42.3737
      NR(IBR) = - IT                                                    M42.3738
      TR(IT) = 0.0                                                      M42.3739
      TX(IT) = 0.0                                                      M42.3740
C  THE FOLLOWING FACTOR OF D10=1.0D+6 IS DUE TO MICRO F OR MHO.         M42.3741
      C(IT) = D10*D10*HJUM/23.73                                        M42.3742
      IF (COPT .NE. 0.0) C(IT) = C(IT)*COPT*TWOPI                       M42.3743
      IF (IPRSUP .GE. 1)                                                M42.3744
     1 WRITE (LUNIT6,17605) MLUM,MBUS(IBR),KBUS(IBR),IBR,IT             M42.3745
17605 FORMAT(10H *********,                                             M42.3746
     1        16H MASS BRANCH NR.,I3,25X,I4,4X,I4,4X,I4,4X,I4)          M42.3747
C  CREATING SPRING ELEMENT :                                            M42.3748
      N6 = NSMTPR + NUMASU + MLUM + 1                                   M42.3749
      IF (MLUM .EQ. NUMASU) GO TO 17608                                 M42.3750
      IF (SPRING .EQ. 0.0) GO TO 17608                                  M42.3751
      CALL IBRINC                                                       VAX.  56
      IT = IT + 1                                                       M42.3753
      KBUS(IBR) = NTOTST + NUMASU + MLUM                                M42.3754
      MBUS(IBR) = NTOTST + MLUM + 1                                     M42.3755
      LENGTH(IBR) = 1                                                   M42.3756
      NR(IBR) = - IT                                                    M42.3757
      TR(IT) = 0.0                                                      M42.3758
C  THE FOLLOWING FACTOR OF 1.0D+3 IS BECAUSE OF MILLI HENRY             M42.3759
      D1 = D10 * SPRING/0.73756                                         M42.3760
      IF (XOPT .EQ. 0.0) TX(IT) = 1.0D+3/D1                             M42.3761
      IF (XOPT .NE. 0.0) TX(IT) = TWOPI*XOPT/D1                         M42.3762
      C(IT) = 0.0                                                       M42.3763
      IF (IPRSUP .GE. 1)                                                M42.3764
     1 WRITE(LUNIT6,17606) MBUS(IBR),KBUS(IBR),IBR,IT                   M42.3765
17606 FORMAT(10H *********,                                             M42.3766
     1        14H SPRING BRANCH,30X,I4,4X,I4,4X,I4,4X,I4)               M42.3767
      UMOUTP(N6) = 1.0/D1                                               M42.3768
      GO TO 17610                                                       M42.3769
17608 UMOUTP(N6) = 0.0                                                  M42.3770
C  CREATING SYNCHRONOUS DAMPING AND SYNCHRONOUS SPEED SOURCE :          M42.3771
17610 IF (DSYNUM .EQ. 0.0) GO TO 17620                                  M42.3772
      NTOT = NTOT + 1                                                   M42.3773
      NRSYN = NRSYN + 1                                                 M42.3774
      KODE(NTOT) = 0                                                    M42.3775
      BUS(NTOT) = TRASH                                                 M42.3776
      IF (NUMUM .GT. 5) GO TO 17614                                     M42.3777
      IF (N5 .NE. 1) GO TO 17612                                        M42.3778
      BUS(NTOT) = TEXTA(NUMUM+65)                                       M42.3779
17612 IF (N5 .NE. 2) GO TO 17614                                        M42.3780
      BUS(NTOT) = TEXTA(NUMUM+70)                                       M42.3781
17614 KCONST = KCONST + 1                                               M42.3782
      IFORM(KCONST) = 14                                                M42.3783
      NODE(KCONST) = NTOT                                               M42.3784
      KODE(NTOT) = - NTOT                                               M42.3785
      SFREQ(KCONST) = 0.00001                                           M42.3786
      TSTART(KCONST) = - 1.0                                            M42.3787
      TSTOP(KCONST) = FLTINF                                            M42.3788
      CREST(KCONST) = ROTMOM(N1)/NPPAIR(N1)                             M42.3789
      IF (IPRSUP .GE. 1)                                                M42.3790
     1 WRITE(LUNIT6,17615) NODE(KCONST),KCONST,SFREQ(KCONST)            M42.3791
17615 FORMAT(10H *********,                                             M42.3792
     1        30H SYNCHR DAMPING VOLTAGE SOURCE,14X,I4,44X,             M42.3793
     2    I4,E14.5)                                                     M42.3794
      CALL IBRINC                                                       VAX.  57
      IT = IT + 1                                                       M42.3796
      KBUS(IBR) = NTOTST + MLUM                                         M42.3797
      MBUS(IBR) = NTOT                                                  M42.3798
      LENGTH(IBR) = 1                                                   M42.3799
      NR(IBR) = - IT                                                    M42.3800
      TR(IT) = 0.73756/DSYNUM                                           M42.3801
      TX(IT) = 0.0                                                      M42.3802
      C(IT) = 0.0                                                       M42.3803
      IF (IPRSUP .GE. 1)                                                M42.3804
     1 WRITE(LUNIT6,17616) MLUM,MBUS(IBR),KBUS(IBR),IBR,IT              M42.3805
17616 FORMAT(10H *********,                                             M42.3806
     1        36H SYNCHR. DAMPING BRANCH FOR MASS NR.,I3,5X,            M42.3807
     2    I4,4X,I4,4X,I4,4X,I4)                                         M42.3808
C  CREATING MUTUAL DAMPING :                                            M42.3809
17620 N6 = NSMTPR + 2*NUMASU + MLUM + 1                                 M42.3810
      IF (MLUM .EQ. NUMASU) GO TO 17628                                 M42.3811
      IF (DMUTUM .EQ. 0.0) GO TO 17628                                  M42.3812
      CALL IBRINC                                                       VAX.  58
      IT = IT + 1                                                       M42.3814
      KBUS(IBR) = NTOTST + NUMASU + MLUM                                M42.3815
      MBUS(IBR) = NTOTST + MLUM + 1                                     M42.3816
      LENGTH(IBR) = 1                                                   M42.3817
      NR(IBR) = - IT                                                    M42.3818
      TR(IT) = 0.73756/DMUTUM                                           M42.3819
      TX(IT) = 0.0                                                      M42.3820
      C(IT) = 0.0                                                       M42.3821
      IF (IPRSUP .GE. 1)                                                M42.3822
     1 WRITE (LUNIT6,17626) MBUS(IBR),KBUS(IBR),IBR,IT                  M42.3823
17626 FORMAT(10H *********,                                             M42.3824
     1      22H MUTUAL DAMPING BRANCH,22X,I4,4X,I4,4X,I4,4X,I4)         M42.3825
      IF (SPRING .EQ. 0.0) GO TO 17628                                  M42.3826
      D1 = 0.73756/(D10*SPRING)                                         M42.3827
      UMOUTP(N6) = D1/TR(IT)                                            M42.3828
      GO TO 17630                                                       M42.3829
17628 UMOUTP(N6) = 0.0                                                  M42.3830
C  CREATING ABSOLUTE DAMPING :                                          M42.3831
17630 IF (DABSUM .EQ. 0.0) GO TO 17640                                  M42.3832
      CALL IBRINC                                                       VAX.  59
      IT = IT + 1                                                       M42.3834
      KBUS(IBR) = NTOTST + MLUM                                         M42.3835
      MBUS(IBR) = 1                                                     M42.3836
      LENGTH(IBR) = 1                                                   M42.3837
      NR(IBR) = - IT                                                    M42.3838
      TR(IT) = 0.73756/DABSUM                                           M42.3839
      TX(IT) = 0.0                                                      M42.3840
      C(IT) = 0.0                                                       M42.3841
      IF (IPRSUP .GE. 1)                                                M42.3842
     1 WRITE(LUNIT6,17638) MLUM,MBUS(IBR),KBUS(IBR),IBR,IT              M42.3843
17638 FORMAT(10H *********,                                             M42.3844
     1        34H ABSLT DAMPING BRANCH FOR MASS NR.,I3,7X,I4,           M42.3845
     2    4X,I4,4X,I4,4X,I4)                                            M42.3846
C  STORING APPLIED TORQUE DISTRIBUTION FACTORS DISTRF IN                M42.3847
C   UMOUTP AND THE CORRESPONDING MASS NODE NRS IN KUMOUT                M42.3848
17640 IF (DISTRF .EQ. 0.0) GO TO 17660                                  M42.3849
      NTORQ = NTORQ + 1                                                 M42.3850
      KUMOUT(NTORQ) = NTOTST + MLUM                                     M42.3851
      IF (NTORQ .NE. 1) GO TO 17650                                     M42.3852
      DISTR1 = DISTRF                                                   M42.3853
C  DISTR1 IS THE FIRST MASS ENCOUNTERED WITH DISTRF .NE. 0.0            M42.3854
      UMOUTP(NSMTPR+1) = 1.0D+3                                         M42.3855
      GO TO 17660                                                       M42.3856
17650 UMOUTP(NSMTPR+NTORQ) = 1.0D+3 * DISTRF/DISTR1                     M42.3857
17660 CONTINUE                                                          M42.3858
C  OUTPUT NAMES OF NODES NEXT TO MEASUREMENT SWITCHES :                 M42.3859
      N6 = NUMASU - 1                                                   M42.3860
      IF (NUMASU .EQ. 1) GO TO 17672                                    M42.3861
      DO 17670 N5 = 1,N6                                                M42.3862
      N7 = NTOTST + NUMASU + N5                                         M42.3863
      N8 = NTOTST + N5 + 1                                              M42.3864
      BUS(N7) = BUS(N8)                                                 M42.3865
17670 CONTINUE                                                          M42.3866
17672 N7 = NTOTST + 2*NUMASU                                            M42.3867
      BUS(N7) = TRASH                                                   M42.3868
      IF (NUMUM .GT. 5) GO TO 17700                                     M42.3869
      N3 = 41 + (NUMUM - 1)*4                                           M42.3870
      BUS(N7) = TEXTA(N3)                                               M42.3871
C  *************************** READ CLASS 5 SM TYPE-59 DATA             M42.3872
17700 CALL CIMAGE                                                       M42.3873
      DECODE (80,17701,ABUFF(1)) TEXT3                                  M42.3874
17701 FORMAT(A6)                                                        M42.3875
      IF (TEXT3 .EQ. BLANK) GO TO 17770                                         
      IF (TEXT3 .EQ. TESM4) GO TO 17750                                 M42.3877
      IF (TEXT3 .EQ. TESM5) GO TO 17750                                 M42.3878
      IF (TEXT3 .EQ. TESM6) GO TO 17750                                 M42.3879
      N15 = 68                                                          M42.3880
      IF (NUMASU .LE. 68) N15 = NUMASU                                  M42.3881
      DECODE (80,17702,ABUFF(1)) NPARUM,N2, N3, N4, N5, N6, N7,         M42.3882
     1 N8, N9, N10, N11, N12, (JUMOUT(I), I=1,N15)                      M42.3883
17702 FORMAT(80I1)                                                      M42.3884
      IF (JTMTAC(N1) .NE. -999999) GO TO 17703                          M42.3885
      DO 97702 I = 1,N15                                                M42.3886
97702 JUMOUT(I) = 0                                                     M42.3887
17703 IF (NOUTPR .EQ. 0)                                                M42.3888
     1 WRITE (KUNIT6, 17704)  NUMUM                                     M42.3889
17704 FORMAT(5H+UM -,I3, 29H   SM-59 CLASS 5, OUTPUT CARD)              M42.3890
      N16 = 0                                                           M42.3891
      N17 = 1                                                           M42.3892
17706 IF (NUMASU .LE. N15) GO TO 17710                                  M42.3893
17708 CALL CIMAGE                                                       M42.3894
      N16 = N16 + 1                                                     M42.3895
      N14 = 68*N17 + (N16 - 1)*80 + 1                                   M42.3896
      N15 = 68*N17 + N16*80                                             M42.3897
      IF (NUMASU .LE. N15) N15 = NUMASU                                 M42.3898
      DECODE (80,17702,ABUFF(1)) (JUMOUT(I), I=N14,N15)                 M42.3899
      IF (JTMTAC(N1) .NE. -999999) GO TO 17709                          M42.3900
      DO 97708 I = N14,N15                                              M42.3901
97708 JUMOUT(I) = 0                                                     M42.3902
17709 IF (NOUTPR .EQ. 0)                                                M42.3903
     1 WRITE (KUNIT6, 17704)  NUMUM                                     M42.3904
      GO TO 17706                                                       M42.3905
17710 IF (N3 .EQ. 0) GO TO 17716                                        M42.3906
      DO 17714 N15 = 1,3                                                M42.3907
17714 JCLOUT(NCLTOT+N15) = 2                                            M42.3908
17716 IF (N2 .NE. 0) KSSOUT = 1                                         M42.3909
      IF (N4 .NE. 0) JCLOUT(NCLTOT+4) = 1                               M42.3910
      IF (N5 .NE. 0) JCLOUT(NCLTOT+5) = 1                               M42.3911
      IF (N6 .NE. 0) JCLOUT(NCLTOT+6) = 1                               M42.3912
      IF (N7 .NE. 0) JCLOUT(NCLTOT+7) = 1                               M42.3913
      IF (N9 .NE. 0) JTQOUT(N1) = 1                                     M42.3914
      IF (N10 .EQ. 1) JTHOUT(N1) = 3                                    M42.3915
      IF (NMEXC .EQ. 0) JTHOUT(N1) = 0                                  M42.3916
      IF (N10 .EQ. 3) JTHOUT(N1) = 1                                    M42.3917
      IF (N3 .NE. 0) GO TO 17726                                        M42.3918
      IF (N11 .EQ. 0) GO TO 17726                                       M42.3919
      DO 17724 N15 = 1,3                                                M42.3920
17724 JCLOUT(NCLTOT+N15) = 1                                            M42.3921
17726 IF (N12 .EQ. 0) GO TO 17728                                       M42.3922
      IF (N12 .EQ. 2) GO TO 17727                                       M42.3923
      JTQOUT(N1) = 2                                                    M42.3924
      JOMOUT(N1) = 2                                                    M42.3925
      GO TO 17728                                                       M42.3926
17727 JTQOUT(N1) = 3                                                    M42.3927
      JOMOUT(N1) = 3                                                    M42.3928
C  INTERFACING IF MASS SPEED OUTPUT IS REQUESTED :                      M42.3929
17728 DO 17734 N15 = 1,NUMASU                                           M42.3930
      IF (JUMOUT(N15) .EQ. 0) GO TO 17734                               M42.3931
      IF (JUMOUT(N15) .EQ. 2) GO TO 17734                               M42.3932
      NV = NV + 1                                                       M42.3933
      IF (NV .LE. LSIZ12) GO TO 17732                                   M42.3934
      WRITE (LUNIT6, 17730) NUMUM                                       M42.3935
17730 FORMAT(/, 36H ERROR STOP.   OUTPUT NV .GT. LSIZ12,                M42.3936
     1          19H FOR SPEEDS OF UM -, I3)                             M42.3937
      GO TO 9600                                                                
17732 IBRNCH(NV) = NTOTST + N15                                         M42.3939
      JBRNCH(NV) = 1                                                    M42.3940
17734 CONTINUE                                                          M42.3941
C  SET MEASUREMENT SWITCHES AND SHAFT TORQUE OUTPUTTING :               M42.3942
C   (KSWSTA+1) IS FIRST SWITCH TO BE SET FROM MASS 1 TO MASS 2          M42.3943
      IF (JTMTAC(N1) .EQ. -999999) GO TO 17800                          M42.3944
      KSWSTA = KSWTCH                                                   M42.3945
      N16 = NUMASU - 1                                                  M42.3946
      IF (NUMASU .EQ. 1) GO TO 17742                                    M42.3947
      DO 17740 N15 = 1,N16                                              M42.3948
      KSWTCH = KSWTCH + 1                                               M42.3949
      NDX1 = KSWTCH + LSWTCH                                            M42.3950
      KMSWIT(KSWTCH) = NTOTST + N15 + NUMASU                            M42.3951
      KMSWIT(NDX1)  =  NTOTST + N15                                     M42.3952
      KSWTYP(KSWTCH) = 0                                                M42.3953
      TCLOSE(KSWTCH) = - 1.0                                            M42.3954
      TOPEN(KSWTCH) = FLTINF                                            M42.3955
      KPOS(KSWTCH) = 11                                                 M42.3956
      IF (JUMOUT(N15) .EQ. 2) KPOS(KSWTCH) = - 11                       M42.3957
      IF (JUMOUT(N15) .EQ. 3) KPOS(KSWTCH) = - 11                       M42.3958
      IF (IPRSUP .GE. 1)                                                M42.3959
     1 WRITE (LUNIT6,17738) KMSWIT(KSWTCH),KMSWIT(NDX1),                M42.3960
     2                      KSWTCH,LSWTCH                               M42.3961
17738 FORMAT(10H *********,                                             M42.3962
     1       21H TORQUE SENSOR SWITCH,23X,I4,4X,I4,20X,I4,4X,I4)        M42.3963
17740 CONTINUE                                                          M42.3964
C  CREATE SMALL SERIES RESISTANCE FOR APPLIED TORQUE TO UM MASS         M42.3965
17742 CALL IBRINC                                                       VAX.  60
      IT = IT + 1                                                       M42.3967
      KBUS(IBR) = NMGEN                                                 M42.3968
      MBUS(IBR) = NTOTST + 2*NUMASU                                     M42.3969
      LENGTH(IBR) = 1                                                   M42.3970
      NR(IBR) = - IT                                                    M42.3971
      TR(IT) = 1.0                                                      M42.3972
      TX(IT) = 0.0                                                      M42.3973
      C(IT) = 0.0                                                       M42.3974
      IF (IPRSUP .GE. 1)                                                M42.3975
     1 WRITE(LUNIT6,17748) MBUS(IBR),KBUS(IBR),IBR,IT                   M42.3976
17748 FORMAT(10H *********,                                             M42.3977
     1        40H SMALL SERIES RESIST FOR APLL GEN TORQUE, 4X,          M42.3978
     2  I4,4X,I4,4X,I4,4X,I4)                                           M42.3979
      GO TO 17800                                                       M42.3980
17750 N15 = 1                                                           M42.3981
      DECODE (80,17752,ABUFF(1)) NPARUM,N2, N3, N4, N5, N6, N7,         M42.3982
     1 N8, N9, N10, N11, N12                                            M42.3983
17752 FORMAT(10X, 12I5)                                                 M42.3984
      IF (NOUTPR .EQ. 0)                                                M42.3985
     1 WRITE (KUNIT6, 17704)  NUMUM                                     M42.3986
      IF (TEXT3 .NE. TESM4) GO TO 17756                                 M42.3987
      IF (JTMTAC(N1) .EQ. -999999) GO TO 17710                          M42.3988
      DO 17754 N15 = 1,NUMASU                                           M42.3989
17754 JUMOUT(N15) = 3                                                   M42.3990
      GO TO 17710                                                       M42.3991
17756 IF (TEXT3 .NE. TESM5) GO TO 17760                                 M42.3992
      DO 17758 N15 = 1,NUMASU                                           M42.3993
17758 JUMOUT(N15) = 0                                                   M42.3994
      GO TO 17710                                                       M42.3995
17760 N15 = 0                                                           M42.3996
      N16 = 0                                                           M42.3997
      N17 = 0                                                           M42.3998
      GO TO 17708                                                       M42.3999
C  IN CASE OF NEW SM-59 OUTPUT REQUEST USAGE :                          M42.4000
17770 IF (NOUTPR .EQ. 0) WRITE(KUNIT6,17772) NUMUM                      M42.4001
17772 FORMAT(5H+UM -,I3,34H   SM-59 CLASS 4, TERMINATION CARD)          M42.4002
17774 CALL CIMAGE                                                       M42.4003
      DECODE(80,17701,ABUFF(1)) TEXT3                                   M42.4004
      IF (TEXT3 .EQ. BLANK) GO TO 17799                                         
      DECODE(80,17776,ABUFF(1)) NGROUP,NALL,(NDUM(I),I=1,12)            M42.4006
17776 FORMAT(2X,2I1,4X,12I6)                                            M42.4007
      IF (NOUTPR .EQ. 0) WRITE(KUNIT6,17778) NUMUM,NGROUP               M42.4008
17778 FORMAT(5H+UM -,I3,30H   SM-59 CLASS 5, OUTPUT GROUP,I2)           M42.4009
      IF (NGROUP .GT. 0 .AND. NGROUP .LE. 4) GO TO 97778                M42.4010
      WRITE (LUNIT6,97777) NGROUP                                       M42.4011
97777 FORMAT(/,43H ERROR STOP. THE LAST-READ CARD IS AN SM-59,          M42.4012
     1        37H OUTPUT REQUEST CARD FOR OUTPUT GROUP,I2,1H.,          M42.4013
     2         39H THIS IS AN ILLEGAL OUTPUT GROUP AS YOU               M42.4014
     3  ,/,    43H COULD FIND OUT BY CONSULTING THE EMTP RULE,          M42.4015
     4     47H BOOK REGARDING THE SM-59 OUTPUT REQUEST RULES.)          M42.4016
      GO TO 9600                                                                
97778 GO TO (17782,17779,17793,17798), NGROUP                           M42.4018
C  TREATMENT OF GROUP 2 (ANGLES) OUTPUT REQUEST : IGNORED               M42.4019
17779 IF (NOUTPR .EQ. 0) WRITE(LUNIT6,17780)                            M42.4020
17780 FORMAT(/,39H WARNING : THIS OUTPUT REQUEST FOR MASS,              M42.4021
     1         34H ANGLES IS IGNORED BY THE UM. THEY,                   M42.4022
     2         40H ARE TO BE OBTAINED FROM THE TACS OUTPUT              M42.4023
     3  ,/,    44H AFTER REQUEST OF TRANSFER INTO TACS THROUGH,         M42.4024
     4         44H THE CLASS-6 SM-59 DATA INPUT CARDS (CONSULT,         M42.4025
     5         16H EMTP RULE BOOK),/)                                   M42.4026
      GO TO 17774                                                               
C   TREATMENT OF GROUP 1 (ELECTRICAL VARIABLES) OUTPUT REQUEST :        M42.4028
17782 IF (NALL .EQ. 0) GO TO 17784                                      M42.4029
      KSSOUT = 1                                                                
      IF (NMEXC .NE. 0) JTHOUT(N1) = 3                                          
      IF (NMEXC .EQ. 0) JTHOUT(N1) = 1                                          
      JTQOUT(N1) = 3                                                            
      JOMOUT(N1) = 3                                                            
      DO 17783 I = 1,7                                                  M42.4035
17783 JCLOUT(NCLTOT+I) = 1                                                      
      GO TO 17774                                                               
17784 DO 17786 I = 1,12                                                         
      IF (NDUM(I) .EQ. 0) GO TO 17786                                   M42.4039
      DO 17785 J = 4,10                                                         
      IF (NDUM(I) .NE. J) GO TO 17785                                   M42.4041
      N2 = J                                                            M42.4042
      IF (J .GT. 7) N2 = J - 7                                          M42.4043
      JCLOUT(NCLTOT+N2) = 1                                             M42.4044
      GO TO 17786                                                       M42.4045
17785 CONTINUE                                                          M42.4046
      IF (NDUM(I) .EQ. 15) JTHOUT(N1) = 3                                       
      IF (NDUM(I) .NE. 14) GO TO 17786                                  M42.4048
      JTQOUT(N1) = 1                                                            
      JOMOUT(N1) = 1                                                            
      IF (JTHOUT(N1) .EQ. 0) JTHOUT(N1) = 1                                     
17786 CONTINUE                                                          M42.4052
      N3 = 0                                                                    
      DO 17792 I = 1,12                                                 M42.4054
      IF (NDUM(I) .EQ. 0) GO TO 17792                                   M42.4055
      DO 17791 J = 1,3                                                          
      IF (NDUM(I) .NE. J) GO TO 17791                                   M42.4057
      IF (JCLOUT(NCLTOT+J) .EQ. 0) GO TO 17790                          M42.4058
      N3 = 1                                                            M42.4059
17788 FORMAT(/,39H WARNING : SIMULTANEOUS REQUEST OF PARK,              M42.4060
     1         41H DOMAIN CURRENTS AND REAL CURRENTS OF THE,            M42.4061
     2         30H POWER WINDINGS OF THIS UM NR.,I3, 7H IS NOT          M42.4062
     3  ,/,    42H HONORED. THE OUTPUT WILL ONLY DISPLAY THE,           M42.4063
     4         15H REAL CURRENTS.,/)                                    M42.4064
      GO TO 17792                                                       M42.4065
17790 JCLOUT(NCLTOT+J) = 2                                                      
      GO TO 17792                                                       M42.4067
17791 CONTINUE                                                          M42.4068
      IF (NDUM(I) .LT. 12) GO TO 17792                                  M42.4069
      IF (NDUM(I) .GT. 13) GO TO 17792                                  M42.4070
      JTQOUT(N1) = 3                                                            
      JOMOUT(N1) = 3                                                            
17792 CONTINUE                                                          M42.4073
      IF (NOUTPR .EQ. 0 .AND. N3 .NE. 0) WRITE(LUNIT6,17788) NUM        M42.4074
      GO TO 17774                                                               
C   TREATMENT OF GROUP 3 (MASS SPEEDS) OUTPUT REQUEST :                 M42.4076
17793 N2 = 2                                                            M42.4077
      N3 = 1                                                            M42.4078
17794 IF (JTMTAC(N1) .NE. -999999) GO TO 17796                          M42.4079
      DO 17795 I = 1,NUMASU                                                     
17795 JUMOUT(I) = 0                                                     M42.4081
      GO TO 17774                                                               
17796 IF (NALL .EQ. 0) GO TO 97797                                      M42.4083
      DO 17797 I = 1,NUMASU                                                     
      IF (JUMOUT(I) .EQ. N2) JUMOUT(I) = 3                                      
      IF (JUMOUT(I) .EQ. 0) JUMOUT(I) = N3                                      
17797 CONTINUE                                                          M42.4087
      GO TO 17774                                                               
97797 DO 97798 I = 1,12                                                         
      N15 = NDUM(I)                                                     M42.4090
      IF (N15 .EQ. 0) GO TO 97798                                       M42.4091
      IF (JUMOUT(N15) .EQ. N2) JUMOUT(N15) = 3                                  
      IF (JUMOUT(N15) .EQ. 0) JUMOUT(N15) = N3                                  
97798 CONTINUE                                                          M42.4094
      GO TO 17774                                                               
C   TREATMENT OF GROUP 4 (TORQUES) OUTPUT REQUEST :                     M42.4096
17798 N2 = 1                                                            M42.4097
      N3 = 2                                                            M42.4098
      GO TO 17794                                                       M42.4099
C   BLANK CARD ENDING SM-59 OUTPUT REQUEST :                            M42.4100
17799 IF (NOUTPR .EQ. 0) WRITE(KUNIT6,97799) NUMUM                      M42.4101
97799 FORMAT(5H+UM -,I3,34H   SM-59 CLASS 5, TERMINATION CARD)          M42.4102
      GO TO 17728                                                       M42.4103
C  *************************** READ CLASS 6 SM TYPE-59 DATA             M42.4104
17800 NANGRE = 0                                                        M42.4105
      NEXCSW = 0                                                        M42.4106
      NTACB = NTOTAC                                                    M42.4107
C  NANGRE IS FIRST ENTRY OF UMOUTP THAT INDICATES ANGLE TRANSFER        M42.4108
      N19 = 0                                                           M42.4109
C  N19 = 1 IS FLAG FOR TACS CONTROL OF TOTAL APPLIED TORQUE.            M42.4110
      NIUNRS = IUTY(KIUTY+1)                                            M42.4111
17810 CALL CIMAGE                                                       M42.4112
      N17 = 0                                                           M42.4113
C  N17 IS IN READING CLASS 6 RESERVED AS FLAG FOR MODIFIED              M42.4114
C    FORMAT OF CODE NUMBERS IF N20 .EQ. 0                               M42.4115
      DECODE (80,17812,ABUFF(1)) N20,BUS1,N2                            M42.4116
17812 FORMAT(I2,A6,I6)                                                  M42.4117
      IF (N20 .EQ. 0) N17 = 3 * LIMASU                                  M42.4118
      IF (N20 .EQ. 0) N20 = N2                                          M42.4119
      IF (N20 .EQ. 0) GO TO 17816                                       M42.4120
      IF (NOUTPR .EQ. 0)                                                M42.4121
     1 WRITE (KUNIT6, 17814)  NUMUM                                     M42.4122
17814 FORMAT(5H+UM -,I3,30H   SM-59 CLASS 6, TACS REQUEST)              M42.4123
17816 IF (BUS1 .NE. TESM7) GO TO 17820                                  M42.4124
      DECODE(80,17817,ABUFF(1)) BUS1,BUS2                               M42.4125
17817 FORMAT(2X,2A6)                                                    M42.4126
      IF (BUS2 .NE. TESM8) GO TO 17818                                  M42.4127
      WRITE (LUNIT6,17053)                                              M42.4128
      GO TO 9600                                                                
17818 IF (NOUTPR .EQ. 0)                                                M42.4130
     1 WRITE (KUNIT6, 17819)  NUMUM                                     M42.4131
17819 FORMAT(5H+UM -, I3, 20H   READING COMPLETED)                      M42.4132
      IF (N19 .EQ. 0) GO TO 17890                                       M42.4133
      GO TO 17900                                                       M42.4134
C  REQUEST FOR TACS CONTROL OF FIELD VOLTAGE :                          M42.4135
17820 N4 = 71 + N17                                                     M42.4136
      IF (N20 .NE. N4) GO TO 17830                                      M42.4137
      DO 17825 N5 = 1,KTAB                                              M42.4138
      NDX1 = ILNTAB( KLNTAB + N5 )                                      M42.4139
      IF (BUS1 .EQ. TEXVEC(NDX1)) GO TO 17827                           M42.4140
17825 CONTINUE                                                          M42.4141
      WRITE (LUNIT6,17826) BUS1                                         M42.4142
17826 FORMAT(/, 40H ERROR STOP.   JUST-READ CARD REQUESTING,            M42.4143
     1          36H CONTROL BY TACS, BEARS UNRECOGNIZED                 M42.4144
     2 ,/,      12H TACS NAME :, A6)                                    M42.4145
      GO TO 9600                                                                
17827 TSTOP(KCONEX) = 0.0                                               M42.4147
C  CREATE TYPE-60 SOURCE :                                              M42.4148
      KCONST = KCONST + 1                                               M42.4149
      IFORM(KCONST) = 60                                                M42.4150
      NODE (KCONST) = NEXC                                              M42.4151
      KODE(NEXC) = - NEXC                                               M42.4152
      BUS(NEXC) = BUS1                                                  M42.4153
      SFREQ(KCONST) = N5                                                M42.4154
      TSTART(KCONST) = 0.0                                              M42.4155
      TSTOP(KCONST) = FLTINF                                            M42.4156
      IF (IPRSUP .GE. 1)                                                M42.4157
     1 WRITE (LUNIT6,17828) NODE(KCONST),KCONST,SFREQ(KCONST)           M42.4158
17828 FORMAT(10H *********,                                             M42.4159
     1        42H TACS ORIGINATED FIELD VOLT TYPE-60 SOURCE,            M42.4160
     2   2X,I4,44X,I4,E14.5)                                            M42.4161
      WRITE (LUNIT6,17829)                                              M42.4162
17829 FORMAT(/, 41H NOTE: THE EMTP CONNECTIVITY LISTING WILL,           M42.4163
     1          44H SHOW ONE OF THE NODES BEARING THE TACS NAME,        M42.4164
     2          41H AS USED IN THE LAST READ CARD. NO REASON            M42.4165
     3   ,/,    42H TO GET ALARMED, SINCE THIS NODE IS ONE OF,          M42.4166
     4          42H THE AUXILIARY NODES CREATED INTERNALLY BY,          M42.4167
     5          15H THE U.M. CODE.,/)                                   M42.4168
      GO TO 17810                                                       M42.4169
C  REQUEST FOR TRANSFER TO TACS OF EXCITER VOLTAGE :                    M42.4170
17830 N4 = 73 + N17                                                     M42.4171
      IF( N20 .NE. N4  .AND.  N20 .NE. (N4+1)  .AND.                    M42.4172
     1 N20 .GT. ( 3*LIMASU ) )   GO  TO  17870                                  
C     LOAD ADDRESS OF BUS1 INTO TACS ARRAY 'UD1' ***************        M42.4174
      NDY5 = KUD1                                                       M42.4175
      DO 15481 IJK = NIUNRS, NIU                                                
      NDX1 = ILNTAB( KALIU+IJK )                                                
      N1IU = IUTY( KIUTY+IJK )                                                  
      IF( N1IU .NE. 92 )   GO  TO  15481                                        
      IF( BUS1 .NE. TEXVEC( NDX1 ) )  GO TO 15481                               
      GO TO 15482                                                               
15481 NDY5 = NDY5 + 5                                                           
      KILL = 108                                                                
      LSTAT( 19 ) = 15481                                               M42.4184
      LSTAT( 15 ) = LBSTAC                                              M42.4185
      LSTAT( 16 ) = NTOTAC                                              M42.4186
      LSTAT( 14 ) = N20                                                 M42.4187
      LSTAT( 17 ) = -1                                                  M42.4188
      LSTAT( 13 ) = 0                                                           
      BUS6 = BUS1                                                               
      JR = 1                                                            M42.4191
      RETURN                                                                    
15482 NTOTAC = NTOTAC + 1                                                       
      UD1( NDY5+2 ) = NTOTAC                                                    
      IF( NTOTAC .LE. LBSTAC )  GO  TO 17834                            M42.4195
      WRITE (LUNIT6,17831)                                              M42.4196
17831 FORMAT(/, 37H ERROR STOP.   OVERFLOW LBSTAC DURING,               M42.4197
     1          34H EXECUTION OF REQUEST IN UMDATA TO)                  M42.4198
      WRITE (LUNIT6,17832) NUMUM                                        M42.4199
17832 FORMAT( /,39H PASS UM VARIABLES TO TACS. THE MACHINE,             M42.4200
     1          38H IN QUESTION, UM -,I3, 12H,IS PROVIDED)              M42.4201
      WRITE (LUNIT6,17833)                                              M42.4202
17833 FORMAT(/, 28H WITH SM TYPE-59 DATA INPUT.)                        M42.4203
      GO TO 9600                                                                
17834 ISMTAC(NTOTAC) = N20                                              M42.4205
      IF (N20 .NE. N4) GO TO 17840                                      M42.4206
      N3 = 3*NUMASU + NSMTAC                                            M42.4207
      UMOUTP(N3+1) = - 5.0                                              M42.4208
      UMOUTP(N3+2) = NEXC                                               M42.4209
      UMOUTP(N3+3) = NTOTAC                                             M42.4210
      NSMTAC = NSMTAC + 3                                               M42.4211
      GO TO 17810                                                       M42.4212
C  REQUEST FOR TRANSFER TO TACS OF EXCITER CURRENT :                    M42.4213
17840 N4 = 74 + N17                                                     M42.4214
      IF (N20 .NE. N4) GO TO 17850                                      M42.4215
C    CREATE ADDITIONAL EXCITER NODE TO INSERT SWITCH                    M42.4216
      NTOT = NTOT + 1                                                   M42.4217
      KODE(NTOT) = 0                                                    M42.4218
      NEXCSW = NTOT                                                     M42.4219
      BUS(NEXCSW) = TRASH                                               M42.4220
      IF (NUMUM .GT. 5) GO TO 17842                                     M42.4221
      N3 = 41 + (NUMUM - 1)*4                                           M42.4222
      BUS(NEXCSW) = TEXTA(N3+3)                                         M42.4223
C    CREATE SWITCH IN EXCITER CIRCUIT                                   M42.4224
17842 KSWTCH = KSWTCH + 1                                               M42.4225
      KSWEXC = KSWTCH                                                   M42.4226
      NDX1 = KSWTCH + LSWTCH                                            M42.4227
      KMSWIT(KSWTCH) = NEXCSW                                           M42.4228
      KMSWIT(NDX1) = NEXC                                               M42.4229
      KSWTYP(KSWTCH) = 0                                                M42.4230
      TCLOSE(KSWTCH) = - 1.0                                            M42.4231
      TOPEN(KSWTCH) = FLTINF                                            M42.4232
      KPOS(KSWTCH) = 11                                                 M42.4233
      IF (IPRSUP .GE. 1)                                                M42.4234
     1 WRITE(LUNIT6,17843) KMSWIT(KSWTCH),KMSWIT(NDX1),                 M42.4235
     2                     KSWTCH,LSWTCH                                M42.4236
17843 FORMAT(10H *********,                                             M42.4237
     1        28H FIELD CURRENT SENSOR SWITCH,16X,I4,4X,I4,20X,         M42.4238
     2    I4,4X,I4)                                                     M42.4239
C    CONNECT EXCITER SERIES RESISTANCE TO NEW SWITCH NODE               M42.4240
      MBUS(IBREXC) = NEXCSW                                             M42.4241
      N3 = 3*NUMASU + NSMTAC                                            M42.4242
      UMOUTP(N3+1) = - 4.0                                              M42.4243
      UMOUTP(N3+2) = KSWEXC                                             M42.4244
      UMOUTP(N3+3) = NTOTAC                                             M42.4245
      NSMTAC = NSMTAC + 3                                               M42.4246
      GO TO 17810                                                       M42.4247
C  TRANSFER TO SOLVUM FOR ANGLE HISTORY CALCULATIONS IF ANGLE           M42.4248
C    TRANSFER TO TACS IS REQUESTED :                                    M42.4249
17850 IF (JTMTAC(N1) .EQ. -999999) GO TO 17810                          M42.4250
      IF (N20 .GT. LIMASU) GO TO 17858                                  M42.4251
C  NOTE : IN CASE THAT JUST ONE MASS ANGLE IS REQUESTED TO BE           M42.4252
C         PASSED TO TACS, THEN THE ANGLE HISTORY OF ALL MASSES          M42.4253
C         NEEDS TO BE TRANSFERED TO SOLVUM.                             M42.4254
      IF (NANGRE .NE. 0) GO TO 17854                                    M42.4255
      NANGRE = NSMTAC + 1                                               M42.4256
      DO 17852 N5 = 1,NUMASU                                            M42.4257
      N3 = 3*NUMASU + NSMTAC                                            M42.4258
      N4 = NSMTPR + N5                                                  M42.4259
      UMOUTP(N3+1) = - 299                                              M42.4260
      UMOUTP(N3+2) = NTOTST + N5                                        M42.4261
      UMOUTP(N3+3) = 0.0                                                M42.4262
      UMOUTP(N3+4) = UMOUTP(N4)                                         M42.4263
      UMOUTP(N3+5) = UMOUTP(NUMASU+N4)                                  M42.4264
      NSMTAC = NSMTAC + 5                                               M42.4265
      IF (N5 .NE. 1) GO TO 17852                                        M42.4266
      UMOUTP(N3+1) = - 300.0 - KSWSTA                                   M42.4267
      UMOUTP(N3+4) = 5.0 * NUMASU                                       M42.4268
      UMOUTP(N3+5) = NUMASU                                             M42.4269
17852 CONTINUE                                                          M42.4270
C  REQUEST FOR TRANSFER TO TACS OF MASS ANGLES                          M42.4271
17854 CONTINUE                                                          M42.4272
      N3 = 3*NUMASU + NANGRE                                            M42.4273
      IF (N20 .EQ. 1) GO TO 17857                                       M42.4274
      N6 = N3 + (N20-1)*5                                               M42.4275
      UMOUTP(N6) = - 3.0                                                M42.4276
17857 N6 = N3 + 2 + (N20-1)*5                                           M42.4277
      UMOUTP(N6) = NTOTAC                                               M42.4278
      GO TO 17810                                                       M42.4279
C  REQUEST FOR TRANSFER TO TACS OF MASS SPEEDS :                        M42.4280
17858 IF (JTMTAC(N1) .EQ. -999999) GO TO 17810                          M42.4281
      N10 = 2*LIMASU                                                    M42.4282
      IF (N20 .GT. N10) GO TO 17860                                     M42.4283
      N3 = 3*NUMASU + NSMTAC                                            M42.4284
      UMOUTP(N3+1) = - 2.0                                              M42.4285
      UMOUTP(N3+2) = NTOTST + N20 - LIMASU                              M42.4286
      UMOUTP(N3+3) = NTOTAC                                             M42.4287
      NSMTAC = NSMTAC + 3                                               M42.4288
      GO TO 17810                                                       M42.4289
C  REQUEST FOR TRANSFER TO TACS OF SHAFT TORQUES :                      M42.4290
17860 IF (JTMTAC(N1) .EQ. -999999) GO TO 17810                          M42.4291
      N10 = 3 * LIMASU                                                  M42.4292
      IF (N20 .GT. N10) GO TO 17870                                     M42.4293
      N3 = 3*LIMASU + NSMTAC                                            M42.4294
      UMOUTP(N3+1) = - 1.0                                              M42.4295
      UMOUTP(N3+2) = KSWSTA + N20 - 2*LIMASU                            M42.4296
      UMOUTP(N3+3) = NTOTAC                                             M42.4297
      NSMTAC = NSMTAC + 3                                               M42.4298
      GO TO 17810                                                       M42.4299
C  SET TYPE-14 AND 18 FOR ELECTROMECH. EXCITER TORQUE FROM TACS:        M42.4300
17870 N4 = 80 + N17                                                     M42.4301
      IF (N20 .NE. N4) GO TO 17880                                      M42.4302
      WRITE (LUNIT6,17872) NUMUM                                        M42.4303
17872 FORMAT(/,45H NOTE : THIS TACS INTERFACE REQUEST REGARDING,        M42.4304
     1         33H THE EXCITER TORQUE FOR UM NUMBER,I4,                 M42.4305
     2         43H,  IS NO LONGER IN EFFECT. THE INFLUENCE OF           M42.4306
     3  ,/,    41H THE EXCITER TORQUE WILL BE AUTOMATICALLY,            M42.4307
     4         41H INCLUDED IN EXACTLY THE SAME WAY AS WITH,            M42.4308
     5         21H THE SM TYPE-59 CODE.,/)                              M42.4309
      GO TO 17810                                                       M42.4310
C  REQUEST FOR TACS CONTROL OF TOTAL APPLIED TORQUE :                   M42.4311
17880 IF (JTMTAC(N1) .EQ. -999999) GO TO 17810                          M42.4312
      N4 = 72 + N17                                                     M42.4313
      IF (N20 .NE. N4) GO TO 17810                                      M42.4314
      N19 = 1                                                           M42.4315
      DO 17882 N10 = 1,KTAB                                             M42.4316
      NDX1 = ILNTAB( KLNTAB + N10 )                                     M42.4317
      IF (BUS1 .EQ. TEXVEC(NDX1)) GO TO 17890                           M42.4318
17882 CONTINUE                                                          M42.4319
      WRITE (LUNIT6,17826) BUS1                                         M42.4320
      GO TO 9600                                                                
C  SET TYPE-14 SOURCES FOR APPLIED TORQUES :                            M42.4322
17890 IF (JTMTAC(N1) .EQ. -999999) GO TO 17899                          M42.4323
      DO 17898 N5 = 1,NTORQ                                             M42.4324
      IF (N19 .NE. 1) GO TO 17894                                       M42.4325
      KCONST = KCONST + 1                                               M42.4326
      SFREQ(KCONST) = N10                                               M42.4327
      IFORM(KCONST) = 17                                                M42.4328
      NODE(KCONST) = - 1                                                M42.4329
      TSTART(KCONST) = - 1.0                                            M42.4330
      TSTOP(KCONST) = FLTINF                                            M42.4331
      IF (IPRSUP .GE. 1)                                                M42.4332
     1 WRITE(LUNIT6,17892) NODE(KCONST),KCONST,SFREQ(KCONST)            M42.4333
17892 FORMAT(10H *********,                                             M42.4334
     1       37H TYPE-17 SOURCE FOR NEXT TACS CONTROL,7X,I4,            M42.4335
     2     44X,I4,E14.5)                                                M42.4336
17894 KCONST = KCONST + 1                                               M42.4337
      IF (N5 .EQ. 1) NODMUM(N1) = KCONST                                M42.4338
      IFORM(KCONST) = 14                                                M42.4339
      N6 = KUMOUT(N5)                                                   M42.4340
      IF (N6 .EQ. NMGEN) N6 = NTOTST + 2*NUMASU                         M42.4341
      NODE(KCONST) = - N6                                               M42.4342
      KODE(N6) = 0                                                      M42.4343
      N4 = NSMTPR + N5                                                  M42.4344
      CREST(KCONST) = UMOUTP(N4)                                        M42.4345
      UMOUTP(N4) = 0.0                                                  M42.4346
      TSTART(KCONST) = - 7777.0                                         M42.4347
      TSTOP(KCONST) = FLTINF                                            M42.4348
      SFREQ(KCONST) = 0.00001                                           M42.4349
      IF (IPRSUP .GE. 1)                                                M42.4350
     1 WRITE(LUNIT6,17896) NODE(KCONST),KCONST,SFREQ(KCONST)            M42.4351
17896 FORMAT(10H *********,                                             M42.4352
     1        22H APPLIED TORQUE SOURCE,22X,I4,44X,I4,E14.5)            M42.4353
17898 CONTINUE                                                          M42.4354
17899 IF (N19 .EQ. 0) GO TO 17900                                       M42.4355
      GO TO 17810                                                       M42.4356
C ********************* FINISH STATEMENTS OF SM TYPE-59 DATA            M42.4357
17900 NCLTOT = NCLTOT + 7                                               M42.4358
      IF (NCLTOT .LE. NCLFIX) GO TO 17905                               M42.4359
      WRITE(LUNIT6,17904) NCLTOT                                        M42.4360
17904 FORMAT( /,  29H OVERFLOW OF U.M. COIL TABLE,,                     M42.4361
     1            10H NCLTOT = , I5, 9H INCREASE,                       M42.4362
     2            29H NCLFIX ON UM DIMENSION CARD.)                     M42.4363
      GO TO 9600                                                                
17905 DO 17910 N5 = 1,NUMASU                                            M42.4365
      KUMOUT(N5) = 0                                                    M42.4366
17910 JUMOUT(N5) = 0                                                    M42.4367
      MJM = NCLTOT - 6                                                  M42.4368
      DO 17920 NK = MJM, NCLTOT                                         M42.4369
17920 JCLTAC(NK) = 0                                                    M42.4370
      IF (IPRSUP .GE. 1)                                                M42.4371
     1 WRITE(LUNIT6,17930) NTOTST,NUMASU,NRSYN,NMGEN,NMEXC              M42.4372
17930 FORMAT(/,10H *********,                                           M42.4373
     1           38H CREATED NODES FOR MECHANICAL SYSTEM :,             M42.4374
     2           41H FIRST NODE NR = NTOTST+1, LAST NODE NR =,          M42.4375
     3           28H NTOTST + 2*NUMASU + NRSYN .                        M42.4376
     4  ,/, 30X,6HNTOTST,2X,6HNUMASU,2X,6H NRSYN,2X,6H NMGEN,2X,        M42.4377
     5  6H NMEXC,/,32X,I4,4X,I4,4X,I4,4X,I4,4X,I4)                      M42.4378
      IF (IPRSUP .GE. 1) WRITE(LUNIT6,17932) NEXC,NEXCSW                M42.4379
17932 FORMAT(/,10H *********,                                           M42.4380
     1           34H CREATED NODES FOR FIELD CIRCUIT :,                 M42.4381
     2            8H NEXC = ,I4,10X, 9HNEXCSW = ,I4)                    M42.4382
C  SHIFTING UMOUTP ENTRIES WITH 3*NUMASU TO BEHIND NSMTPR :             M42.4383
      N6 = NSMTAC - NSMTPR                                              M42.4384
      N7 = NSMTPR + N6                                                  M42.4385
      DO 17950 N5 = 1,N6                                                M42.4386
      N3 = NSMTPR + N5                                                  M42.4387
      N4 = N3 + 3*NUMASU                                                M42.4388
      UMOUTP(N3) = UMOUTP(N4)                                           M42.4389
      IF (N4 .GT. N7) UMOUTP(N4) = 0.0                                  M42.4390
17950 CONTINUE                                                          M42.4391
      IF (NPARUM .NE. 0) GO TO 17951                                    M42.4392
      IF (IPRSUP .LT. 3) GO TO 17958                                    M42.4393
17951 WRITE (LUNIT6,17952) N1,REAMDU(N1),REAMDS(N1),                    M42.4394
     1    REAMQU(N1),REAMQS(N1),FLXDS(N1),FLXQS(N1)                     M42.4395
17952 FORMAT(/, 21H PARAMETERS OF UM NR.,I4,1H:,10X,4HLMUD,             M42.4396
     1  10X,4HLMSD,10X,4HLMUQ,10X,4HLMSQ,9X,5HFLXSD,9X,5HFLXSQ          M42.4397
     2   ,/, 26X,6E14.5)                                                M42.4398
      WRITE (LUNIT6,17953)                                              M42.4399
17953 FORMAT(/, 42H COIL     RESISTANCE    LEAKAGE INDUCTANCE)          M42.4400
      N6 = NCLTOT - 6                                                   M42.4401
      DO 17957 N5 = N6,NCLTOT                                           M42.4402
      IF (GPAR(N5) .EQ. 0.0) WRITE (LUNIT6,17954) N5                    M42.4403
17954 FORMAT(1X,I4,10X, 11H DUMMY COIL)                                 M42.4404
      IF (GPAR(N5) .EQ. 0.0) GO TO 17957                                M42.4405
      WRITE (LUNIT6,17955) N5,GPAR(N5),REACL(N5)                        M42.4406
17955 FORMAT(1X,I4,2X,2E14.5)                                           M42.4407
      IF (N5 .EQ. NCLTOT) WRITE(LUNIT6,17956)                           M42.4408
17956 FORMAT(/, 1H )                                                    M42.4409
17957 CONTINUE                                                          M42.4410
C  SET 0.5*RF FOR INTERNAL FIELD RESISTANCE :                           M42.4411
17958 GPAR(NCLTOT-3) = 0.5 * GPAR(NCLTOT-3)                             M42.4412
      N5 = NSMTAC + 3*NUMASU                                            M42.4413
      IF (N5 .LE. IOTFIX) GO TO 17970                                   M42.4414
      WRITE (LUNIT6, 17960)                                             M42.4415
17960 FORMAT( /, 38H OVERFLOW OF UM OUTPUT TABLE, INCREASE,             M42.4416
     1           35H IN CARD FOR ABSOLUTE UM DIMENSIONS                 M42.4417
     2  ,/,      17H VALUE OF IOTFIX.                                   M42.4418
     3  ,/,      37H REMARK : IF SM TYPE-59 DATA INPUT IS,              M42.4419
     4           30H INCLUDED, THEN IOTFIX IS ALSO                      M42.4420
     5  ,/,      36H RELATED TO OUTPUTS OF MECH. SYSTEMS,               M42.4421
     6           33H IN ALL SM TYPE-59 DATA MACHINES.)                  M42.4422
      GO TO 9600                                                                
17970 JF = 1                                                            M42.4424
      RETURN                                                            M42.4425
C  END READING SM TYPE-59 DATA INPUT $$$$$$$$$$$$$$$$$$$$$$$$$$         M42.4426
 9600 CALL STOPTP                                                               
      RETURN                                                                    
      END                                                               M42.4429
      SUBROUTINE SMDAT (MTYPE)                                          M20.1316
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS MODULE IS USED ONLY BY TYPE-59 BRANDWAJN  S.M.  MODEL        M31. 879
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'SYNCOM.FOR;/NOLIST'                                           
      INCLUDE  'SYNMAC.FOR;/NOLIST'                                           
C     THE FOLLOWING ARRAY IS SIZED 3 (NN4-1) * N50 ( N50 = 50  AT PRE-  M39.1266
C     SENT ) + NN10 ( NO. OF CLASS 1 REQUESTS <=15 AT PRESENT> ) ****** M39.1267
      DIMENSION   NPBUF( 165 )                                                  
      EQUIVALENCE (ISMDAT(22),IPOUT), (ISMDAT(23),N56), ( ISMDAT(24),   M39.1269
     1 ISMOLD), (ISMDAT(25),NN10), (ISMDAT(26),NN4), (ISMDAT(27),NN14)  M37.2652
      DIMENSION MASSEX(1)                                               M38.1420
      EQUIVALENCE (HISTQ(1),MASSEX(1))                                  M38.1421
      REAL*8         TEXT1, TEXT2, TEXT3                                M33. 322
      REAL*8         TEXT7, TEXT8,        TEXT10, TEXT11, TEXT12        M28.4229
      REAL*8         TEXT16, TEXT17                                     M33. 323
      REAL*8         TEXT18, TEXT19, TEXT20, TEXT21                     M22.3060
      DATA  TEXT1   / 6HFINISH  /                                       M20.1326
      DATA  TEXT2  / 6HTOLERA /                                         M20.1327
      DATA  TEXT3  / 6HNCES   /                                         M20.1328
      DATA  TEXT7  / 6HPARAME /                                         M20.1332
      DATA  TEXT8  / 6HTER    /                                         M22.3061
      DATA  TEXT10  / 6HDELTA  /                                        M20.1335
      DATA  TEXT11  / 6HCONNEC /                                        M20.1336
      DATA  TEXT12  / 6HTION   /                                        M20.1337
      DATA  TEXT17  / 6HFITTIN /                                        M22.3063
      DATA  TEXT18  / 6HG      /                                        M22.3064
      DATA  TEXT19  / 6HT      /                                        M22.3065
      DATA  TEXT20  / 6HPF     /                                        M22.3066
      DATA  TEXT21  / 6HDC     /                                        M22.3067
      DATA  TEXT16  / 6H PART  /                                        M20.1341
      IF ( IPRSUP  .GE.  1 )                                            M32.2816
     1 WRITE ( LUNIT6, 4567 )                                           M32.2817
 4567 FORMAT ( 23H  "BEGIN MODULE SMDAT." )                             M32.2818
C     DEFINE NO. OF OUTPUTS IN CLASS 1 (NN10) AND NO. OF CLASSES (NN4)**M33. 324
      NN10 = 15                                                         M33. 325
      NN4 = 4                                                           M33. 326
      NN14 = 14                                                         M33. 327
      N50 = 50                                                                  
      NRIGHT = 0                                                        M20.1344
      NFRFLD = 1                                                        M20.1345
      D10  =  1.0  / FLTINF                                             M20.1346
      IF ( NUMSM .GT. 1 ) GO TO 123                                     M20.1347
      CALL RINFIN                                                       M43. 847
      N19 = LOCF( FLSTAT(1) ) - LOCF(  VOLTBC(1) )                      M33. 331
      IF(  N19  .GE. 41 )     GO  TO  8258                              M38.1423
      KILL = 180                                                        M33. 333
      LSTAT( 18 ) = NCHAIN                                              M33. 334
      LSTAT( 19 ) = 8258                                                M33. 335
      LSTAT( 17 ) = 41                                                  M38.1424
      LSTAT( 16 ) = N19                                                 M33. 337
      GO TO 9999                                                        M33. 338
 8258 D8  =  3.0 / 2.0                                                  M38.1425
      THTW  =  SQRTZ( D8 )                                              M20.1349
      ATHTW = 1.0 / THTW                                                M33. 340
      D8 = 3.0                                                          M33. 341
      SQRT3 = SQRTZ( D8 )                                               M33. 342
      ASQRT3 = 1.0 / SQRT3                                              M33. 343
      SQRT32 = SQRT3 * ONEHAF                                           M33. 344
      OMDT = OMEGA * DELTAT                                             M33. 345
      RADEG = 360.0 / TWOPI                                             M33. 346
      BDAM = 1.356306D0 * TENM6                                         M33. 347
      BIN = .0421409745D0                                               M33. 348
      NST = 0                                                           M20.1350
      ITOLD = IT                                                        M20.1351
      IBROLD = IBR                                                      M20.1352
      ISMOLD = 0                                                        M33. 349
      IPOUT = 0                                                                 
      N56 = ( NN4-1 ) * N50 + NN10                                              
      MFIRST = 6 * LIMASS                                               M38.1427
      D8 = NBYTE(3)                                                             
      D8 = D8 / NBYTE(4)                                                        
      OM2 = D8                                                                  
      D8 = MFIRST * D8 - 1.0                                                    
      MFIRST = D8                                                               
 123  JK = NUMSM                                                        M20.1353
      ISM = 0                                                           M38.1433
      FM = -2.0                                                         M38.1434
      K = NST + 1                                                       M38.1435
      IF( K .LE. LSYN )  GO  TO  8818                                   M38.1436
      KILL = 1                                                          M38.1437
      LSTAT( 19 ) = 123                                                 M38.1438
      LSTAT( 16 ) = 17                                                  M38.1439
      GO TO 9999                                                        M38.1440
 8818 J30 = 30 * JK - 29                                                        
      ISMDAT( J30 ) = 0                                                         
      STATFR = SFREQ( KCONST-1 )                                        M33. 351
      IDELTA = 0                                                                
      IF ( IPRSUP  .GE.  1 )                                            M20.1367
     1 WRITE (LUNIT6, 3604)  K,  NST, MTYPE, KCONST, NUMSM              M20.1368
 3604 FORMAT( /, 26H AT START OF  'SMDAT'  ...,                         M33. 352
     1  40H       K     NST   MTYPE  KCONST   NUMSM  ,/,  24X, 10I8 )   M20.1370
C     READ INPUT CARD USING CIMAGE                                      M20.1371
 1    CALL CIMAGE                                                       M20.1372
      IF ( KILL  .GT.  0 )   GO TO 9999                                 M32.2822
      NRIGHT = -2                                                       M22.3069
      N11 = KOLBEG                                                      M22.3070
      KOLBEG = 1                                                        M22.3071
      CALL FREONE ( D1 )                                                M29.1475
      NRIGHT = 0                                                        M22.3073
C     CHECK FOR KEY WORD   'TOLERANCES'  (OR  'T' ).                    M22.3074
      IF ( TEXTA6(1)  .EQ.  TEXT19 )   GO TO 7103                       M38.1442
      IF ( TEXTA6(1)  .NE.  TEXT2 )   GO TO  7106                       M38.1443
      IF ( TEXTA6(2)  .NE.  TEXT3 )   GO TO  7106                       M38.1444
 7103 IF ( N11  .GT.  0 )   GO TO 7104                                  M38.1445
      DECODE (80, 3609, ABUFF(1) )  D1,  D2,  D3,  N9                   M43. 848
 3609 FORMAT ( 10X, 3E10.0, 10X, I10 )                                  M43. 849
      GO TO 7105                                                        M20.1380
 7104 NFRFLD = 1                                                        M22.3079
      CALL FREONE ( D1 )                                                M29.1476
      CALL FREONE ( D2 )                                                M29.1477
      CALL FREONE ( D3 )                                                M29.1478
      CALL FREFLD ( VOLTBC(1) )                                         M31. 883
      N9 = VOLTBC(1)                                                    M31. 884
 7105 IF ( D1  .GT.  0.0 )   EPSUBA = D1                                M20.1389
      IF ( D2  .GT.  0.0 )   EPOMEG = D2                                M20.1390
      IF ( D3  .GT.  0.0 )   EPDGEL = D3                                M20.1391
      IF ( N9  .GT.  0 )   IPRSOV(37) = N9                              M39.1273
      IF ( NOUTPR  .EQ.  0 )                                            M22.3080
     1 WRITE (KUNIT6, 3610)  EPSUBA, EPOMEG, EPDGEL, IPRSOV(37)         M39.1274
 3610 FORMAT ( 11H+  EPSILON.,  3E11.2,  I5  )                          M39.1275
      GO TO 1                                                           M20.1396
C     CHECK FOR KEY WORD   'PARAMETER FITTING'  (OR  'PF' ).            M22.3082
 7106 IF ( TEXTA6(1)  .EQ.  TEXT20 )   GO TO 2115                       M38.1446
      IF ( TEXTA6(1)  .NE.  TEXT7  )   GO TO 8641                       M22.3084
      IF ( TEXTA6(2)  .NE.  TEXT8  )   GO TO 8641                       M38.1447
      IF ( TEXTA6(3)  .NE.  TEXT17 )   GO TO 8641                       M22.3086
      IF ( TEXTA6(4)  .NE.  TEXT18 )   GO TO 8641                       M22.3087
 2115 IF ( N11  .GT.  0 )   GO TO 7109                                  M38.1448
      DECODE (80, 6011, ABUFF(1) )   FM                                 M20.1401
 6011 FORMAT ( 24X, E8.0 )                                              M20.1402
      GO TO 7110                                                        M20.1403
 7109 NFRFLD = 1                                                        M22.3089
      CALL FREONE ( FM )                                                M29.1479
 7110 IF( FM  .GT.  1.0  )  GO  TO  152                                 M20.1406
      ISM  =  1                                                         M20.1407
      GO  TO  153                                                       M20.1408
 152  ISM  =  0                                                         M20.1409
      FM  =  1.0                                                        M20.1410
 153  IF ( NOUTPR  .EQ.  0 )                                            M38.1449
     1 WRITE( KUNIT6, 3617 )  FM                                        M35. 961
 3617 FORMAT (  24H+  OPTIMIZE PARK'S DATA.,  3X,  E12.3  )             M20.1412
      GO TO 1                                                           M20.1413
C     CHECK FOR KEY WORD   'DELTA CONNECTION'  (OR  'DC' ).             M22.3092
 8641 IF ( TEXTA6(1)  .EQ.  TEXT21 )   GO TO 2125                       M22.3093
      IF ( TEXTA6(1)  .NE.  TEXT10 )   GO TO 8659                       M22.3094
      IF ( TEXTA6(2)  .NE.  TEXT11 )   GO TO 8659                       M22.3095
      IF ( TEXTA6(3)  .NE.  TEXT12 )   GO TO 8659                       M22.3096
 2125 IF ( NOUTPR  .EQ.  0 )                                            M22.3097
     1 WRITE (KUNIT6, 8646)                                             M35. 962
 8646 FORMAT (  44H+  NOTIFICATION OF DELTA-CONNECTED ARMATURE.      )  M20.1418
      IDELTA = 1                                                        M37.2660
      GO TO 1                                                           M20.1420
 8659 KOLBEG = N11                                                      M22.3099
      ISMDAT( J30 ) = ISMDAT( J30 ) + 1                                         
      NST = NST + 1                                                     M38.1450
      IF( NST .LE. LSYN )  GO  TO  8819                                 M38.1451
      KILL = 1                                                          M38.1452
      LSTAT( 19 ) = 8659                                                M38.1453
      LSTAT( 16 ) = 17                                                  M38.1454
      GO TO 9999                                                        M38.1455
 8819 K = NST                                                           M38.1456
      I101 = 101 * K - 100                                                      
      I26  = I101                                                               
      I30 = 30 * K - 29                                                         
      ISMDAT( I30+1 ) = IDELTA                                                  
      IF ( KOLBEG  .GT.  0 )   GO TO 7113                               M20.1425
      DECODE( 80, 5, ABUFF(1) )                                         M37.2668
     1 ISMDAT( I30+11 ), ISMDAT( I30+12 ), ISMDAT( I30+13 ), NP,        M37.2669
     2 SM3, SM4, RMVA, RKV, AGLIN, DAB, DAC                             M37.2670
 5    FORMAT( 3I2, I4, 7E10.6 )                                         M37.2671
      GO TO 7114                                                        M20.1430
 7113 NFRFLD = 4                                                        M20.1431
      CALL FREFLD ( VOLTBC(1) )                                         M31. 885
      ISMDAT( I30+11 ) = VOLTBC(1)                                              
      ISMDAT( I30+12 ) = VOLTBC(2)                                              
      ISMDAT( I30+13 ) = VOLTBC(3)                                              
      NP = VOLTBC( 4 )                                                  M33. 354
      NFRFLD = 1                                                        M20.1437
      CALL FREONE ( SM3 )                                               M29.1480
      CALL FREONE ( SM4 )                                               M29.1481
      CALL FREONE ( RMVA )                                              M29.1482
      CALL FREONE ( RKV )                                               M29.1483
      CALL FREONE ( AGLIN )                                             M29.1484
      CALL FREONE( DAB )                                                M35. 963
      CALL FREONE( DAC )                                                M35. 964
 7114 IF ( NOUTPR  .EQ.  0 )                                            M22.3100
     1 WRITE (KUNIT6,3619) ISMDAT( I30+11 ), ISMDAT( I30+12 ),          M37.2675
     2 ISMDAT( I30+13 ), NP, SM3, SM4                                   M37.2676
 3619 FORMAT ( 17H+  4TH S.M. CARD.,  4I4,  2F7.3  )                    M20.1447
      ELP( I26+25 ) = NP / 2                                                    
      IF( SM3  .EQ.  0.0  )    SM3 = D10                                M38.1461
      IF( SM4  .EQ.  0.0  )    SM4 = D10                                M38.1462
      ELP( I26+28 ) = SM3                                                       
      ELP( I26+29 ) = SM4                                                       
      NUMASK = ISMDAT( I30+11 )                                                 
 300  IF( NUMASK .LT. 0  )  GO  TO  306                                 M38.1466
      ILN = ISMOLD * 12                                                         
      ISMOLD = ISMOLD + NUMASK                                          M33. 361
      IF( ISMOLD .LE. LIMASS )  GO TO 8                                 M33. 362
      KILL = 1                                                          M38.1468
      LSTAT( 19 ) = 300                                                 M38.1469
      LSTAT( 16 ) = 16                                                  M38.1470
      GO TO 9999                                                        M38.1471
 306  KILL = 105                                                        M38.1472
      LSTAT( 19 ) = 306                                                 M38.1473
      LSTAT( 14 ) = NUMASK                                              M38.1474
      LSTAT( 18 ) = NCHAIN                                              M33. 363
      GO TO 9999                                                        M22.3102
 8    ISMDAT( I30+8 ) = 0                                                       
      IF( AGLIN .GT. 0.0 )   GO  TO  150                                M23.1161
      ISMDAT( I30+8 ) = 2                                                       
 150  AGLIN = ABSZ( AGLIN )                                             M23.1168
C     READ  INPUT  CARDS  USING  CIMAGE                                 M23.1169
      CALL  CIMAGE                                                      M23.1170
      DECODE(80,163,ABUFF(1) )    AD1,AD2,AQ1,AQ2,QAA,QAB,QAC           M38.1475
 163  FORMAT( 10X, 7E10.6 )                                             M38.1476
      IF( NOUTPR  .EQ.   0   )                                          M23.1173
     1 WRITE( KUNIT6, 154 )  AQ1, QAA, QAB, QAC                         M38.1477
 154  FORMAT( 9H+  Q-AXIS,  F7.3, 3F10.4  )                             M38.1478
C     READ INPUT CARD USING CIMAGE.                                     M28.4236
      CALL CIMAGE                                                       M28.4237
 5564 ZB = ( RKV**2 ) / RMVA                                            M28.4242
C     IF( IDELTA  .EQ.  1  )  ZB = 3.D0 * ZB                            M39.1277
      EMF = RKV / ( TENM3*AGLIN )                                       M38.1479
      IF( FM.LE.-1.0) GO TO 100                                         M20.1466
      IF ( KOLBEG  .GT.  0 )   GO TO 7124                               M20.1469
      DECODE (80, 6, ABUFF(1) )                                         M20.1470
     1 RA, XL, XD, XQ, XDP, XQP, XDPP, XQPP                             M37.2686
    6 FORMAT(8E10.6)                                                    M20.1472
      GO TO 7125                                                        M20.1473
 7124 CALL FREONE ( RA )                                                M37.2687
      CALL FREONE ( XL )                                                M29.1485
      CALL FREONE ( XD )                                                M29.1486
      CALL FREONE ( XQ )                                                M29.1487
      CALL FREONE ( XDP )                                               M29.1488
      CALL FREONE ( XQP )                                               M29.1489
      CALL FREONE ( XDPP )                                              M29.1490
      CALL FREONE ( XQPP )                                              M29.1491
 7125 IF ( NOUTPR  .EQ.  0 )                                            M22.3103
     1 WRITE (KUNIT6, 3628)  RA, XL, XD                                 M37.2688
 3628 FORMAT ( 17H+  5TH S.M. CARD.,  3F10.4  )                         M20.1483
C     CHECK FOR AN ERROR IN REACTANCE VALUES ***************************M20.1484
      IF ( RA  .LT.  0.0 )   GO TO 19                                   M37.2689
      IF ( XL  .LT.  0.0 )   GO TO 19                                   M20.1486
      IF ( XD  .LT.  0.0 )   GO TO 19                                   M20.1487
      IF ( XQ  .LT.  0.0 )   GO TO 19                                   M20.1488
      IF ( XDP  .LT.  0.0 )   GO TO 19                                  M20.1489
      IF ( XQP  .LT.  0.0 )   GO TO 19                                  M20.1490
      IF ( XDPP  .LT.  0.0 )   GO TO 19                                 M20.1491
      IF ( XQPP  .LT.  0.0 )   GO TO 19                                 M20.1492
      IF((XD-XDP).LE.FLZERO.AND.FM.EQ.0.0) GO TO 19                     M20.1493
      IF((XDP-XDPP).LE.FLZERO) GO TO 19                                 M20.1494
      IF((XDPP-XL).LE.FLZERO)  GO TO 19                                 M20.1495
      NDWQA = 0                                                         M23.1178
      IF( XQ .EQ. XQP .AND.  XQP .EQ. XQPP ) NDWQA = -2                 M37.2690
      IF( NDWQA .LT. 0 )   GO TO 25                                     M37.2691
      IF((XQ-XQP).LE.FLZERO.AND.FM.EQ.0.0) GO TO 19                     M20.1496
      IF((XQP-XQPP).LE.FLZERO) GO TO 19                                 M20.1497
      IF((XQPP-XL).LE.FLZERO)  GO TO 19                                 M20.1498
      GO TO 25                                                          M20.1499
   19 KILL = 176                                                        M20.1500
      GO TO 9999                                                        M22.3105
 25   CONTINUE                                                          M20.1502
C     CONVERT MACHINE PARAMETERS TO PHYSICAL VALUES ,THIS WILL  RESULT  M39.1278
C     IN ALL DATA BEING REFERRED TO THE ROTOR SIDE ******************** M39.1279
      XQ = XQ * ZB                                                      M39.1280
      XD = XD * ZB                                                      M39.1281
      XL = XL * ZB                                                      M39.1282
      RA = RA * ZB                                                      M37.2693
      IF ( IPRSUP  .GT.  0  )                                           M37.2694
     1 WRITE (KUNIT6, 3628)  RA, XL, XD                                 M37.2695
C     READ INPUT CARD USING CIMAGE                                      M20.1515
      CALL CIMAGE                                                       M20.1516
      IF ( KOLBEG  .GT.  0 )   GO TO 7136                               M20.1517
      DECODE (80, 30, ABUFF(1) )                                        M20.1518
     1 TDOP, TQOP, TDOPP, TQOPP, EL2, R1, EL1                           M37.2696
 30   FORMAT (7E10.6,9X,I1)                                             M20.1520
      GO TO 7137                                                        M20.1521
 7136 CALL FREONE ( TDOP )                                              M29.1492
      CALL FREONE ( TQOP )                                              M29.1493
      CALL FREONE ( TDOPP )                                             M29.1494
      CALL FREONE ( TQOPP )                                             M29.1495
      CALL FREONE ( EL2 )                                               M29.1496
      CALL FREONE ( R1 )                                                M29.1497
      CALL FREONE ( EL1 )                                               M29.1498
 7137 IF ( NOUTPR  .EQ.  0 )                                            M20.1529
     1 WRITE (KUNIT6, 3637)  TDOP, TQOP, TDOPP                          M35. 969
 3637 FORMAT ( 17H+  6TH S.M. CARD.,  3F10.4  )                         M20.1531
C     PROCESS MANUFACTURER'S DATA ,START WITH THE D-AXIS ************** M37.2697
      X1(1) = XD                                                        M39.1283
      X1(2) = XDP * ZB                                                  M39.1284
      X1(3) = XDPP * ZB                                                 M39.1285
      X1(4) = TDOPP * OMEGA                                             M37.2701
      X1(5) = TDOP * OMEGA                                              M37.2702
      X1(6) = XL                                                        M37.2703
      CALL SMPFIT( X1(1), ISM, FM, LUNIT6, NOUTPR )                     M37.2704
C     CALCULATE TRANSFORMER RATIO FOR SCALING OF ROTOR CIRCUITS ********M20.1555
      A = X1(2) / THTW                                                  M37.2705
      RAT = EMF / A                                                     M38.1480
      RAT2 = 2.0 * RAT**2 / 3.0                                         M20.1558
      A=EMF*RAT/THTW                                                    M20.1559
C     LOAD PROCESSED DATA INTO PERMANENT STORAGE ********************** M37.2706
      IB2 = 20                                                          M37.2707
      CALL MOVER0( ELP( I26 ), IB2 )                                            
      ELP( I26 ) = XD                                                           
      ELP( I26+1 ) = EMF                                                        
      IF( X1(5) .EQ. 0 ) GO TO 6014                                             
      ELP( I26+2 ) = X1(3) * RAT2                                               
      ELP( I26+3 ) = EMF                                                        
      ELP( I26+4 ) = A                                                          
      ELP( I26+5 ) = X1(4) * RAT2                                               
      ELP( I26+6 ) = X1(5) * RAT2                                               
      ELP( I26+7 ) = X1(6) * RAT2                                               
      GO TO 6015                                                        M20.1568
 6014 ELP( I26+2 ) = X1(4) * RAT2                                               
      ELP( I26+5 ) = RAT2                                                       
      ELP( I26+6 ) = X1(6) * RAT2                                               
 6015 RAT1 = EMF / X1( 2 )                                                      
C     QUADRATURE AXIS PARAMETERS ***************************************M20.1577
      ELP( I26+8 ) = XQ                                                         
      IF( NDWQA .LT. 0 )  GO TO 6013                                            
      X1(1) = XQ                                                        M37.2724
      X1(2) = XQP * ZB                                                  M39.1286
      X1(3) = XQPP * ZB                                                 M39.1287
      X1(4) = TQOPP * OMEGA                                             M37.2727
      X1(5) = TQOP * OMEGA                                              M37.2728
      X1(6) = XL                                                        M37.2729
      CALL SMPFIT( X1(1), ISM, FM, LUNIT6, NOUTPR )                     M37.2730
      A = X1(2) * RAT / THTW                                            M37.2731
      ELP( I26+9 ) = A                                                          
      IF( X1(5) .EQ. 0. )  GO TO 71                                     M37.2733
      ELP( I26+10 ) = X1(3) * RAT2                                              
      ELP( I26+11 ) = A                                                         
      ELP( I26+12 ) = X1(2) * RAT2                                              
      ELP( I26+13 ) = X1(4) * RAT2                                              
      ELP( I26+14 ) = X1(5) * RAT2                                              
      ELP( I26+15 ) = X1(6) * RAT2                                              
      GO TO 70                                                          M32.2846
 71   ELP( I26+10 ) = X1(4) * RAT2                                              
      ELP( I26+13 ) = RAT2                                                      
      ELP( I26+14 ) = X1(6) * RAT2                                              
      GO TO 70                                                          M32.2850
 6013 ELP( I26+10 ) = RAT2                                                      
      ELP( I26+13 ) = RAT2                                                      
C     STORE REMAINING VARIABLES *************************************** M37.2745
 70   ELP( I26+16 ) = EL2 * ZB  + 3.0 * EL1                                     
      ELP( I26+17 ) = RA  +  3.0 * R1                                           
      ELP( I26+18 ) = XL                                                        
      ELP( I26+19 ) = RA                                                        
      ELP( I26+20 ) = RAT1                                                      
C     CALCULATE AND STORE SATURATION CONSTANTS ************************ M37.2751
      IF( ISMDAT( I30+8 ) .EQ. 0 )   GO  TO 158                                 
C     START   WITH   THE  D-AXIS   CHARACTERISTIC***********************M23.1197
C     CHECK FOR INCONSISTENT SATURATION DATA ***************************M23.1198
      IF( DAB .GE. AGLIN .AND. DAC .GT. DAB )  GO  TO  167              M33. 365
      KILL = 211                                                        M23.1200
      LSTAT( 14 ) = K                                                   M23.1201
      LSTAT( 19 ) = 167                                                 M23.1202
      FLSTAT( 13 ) = AGLIN                                              M33. 366
      FLSTAT( 14 ) = DAB                                                M33. 367
      FLSTAT( 15 ) = DAC                                                M33. 368
      GO TO 9999                                                        M33. 369
 167  IF( AD1 .LE. 0.0 )    AD1 = 1.0                                           
      IF( AD2 .LE. 0.0 )    AD2 = 1.2                                           
      SF5 = DAB / ( AGLIN * AD1 )                                       M38.1483
      SF2 = DAC / ( AGLIN * AD2 )                                       M38.1484
      SF3 = ( SF2 - SF5 ) / ( DAC - DAB )                               M33. 372
      SF6 = DAC + ( 1.0 - SF2 ) / SF3                                   M33. 373
      IF( SF6 .GT. (AGLIN * ONEHAF ) )  GO  TO  437                             
C     RESET SATURATION PARAMETERS ************************************* M39.1289
      DSM = RKV / AGLIN                                                 M39.1290
      HSP = RKV * ( AD2 - AD1 ) / ( DAC - DAB )                         M39.1291
      DSD = RKV * AD1 - HSP * DAB                                       M39.1292
      DSR = DSD / ( DSM - HSP )                                                 
      ILV = 167                                                         M39.1294
      WRITE( LUNIT6, 6110 )  ILV, SF6, DSR                              M39.1295
 6110 FORMAT( 32H  ****** WARNING, WARNING ******,                      M39.1296
     1 /, 43H  SUBROUTINE SMDAT, NEARBY STATEMENT NUMBER, I8,           M39.1297
     2 /, 32H  SATURATION TRESHOLD RESET FROM, 2X, E15.8, 3X, 2HTO,     M39.1298
     3 2X, E15.8 )                                                      M39.1299
      SF6 = DSR                                                         M39.1300
      SF3 = ( 1.0 - SF2 ) / ( SF6 - DAC )                                       
 437  ELP( I26+21 ) = SF6                                                       
      ELP( I26+22 ) = SF3                                                       
      IF( IPRSUP .GE. 1 )                                               M23.1212
     1 WRITE(LUNIT6,155) K,SF3,SF6,SF5,SF2                              M37.2755
 155  FORMAT(1X,22H SATURATION CONSTANTS.,7X,1HK,11X,3HSF3,11X,3HSF6,11XM37.2756
     1,3HSF5,11X,3HSF2,/,22X,I8,4E14.5,/,1X)                            M23.1215
C     CALCULATE  PARAMETERS  FOR  THE  Q-AXIS **************************M23.1216
      IF( QAA .GT.  0.0  )   GO  TO 157                                 M23.1218
C     UNKNOWN   PARAMETERS-CALCULATE AN  APPROXIMATE CHARACTERISTIC ****M23.1219
      B6 = ( ELP( I26+8 ) - XL )  / ( ELP( I26 ) - XL )                 M37.2757
      SF6 = SF6 * B6                                                    M37.2758
      ELP( I26+23 ) = SF6                                                       
      ELP( I26+24 ) = SF3                                                       
      IF( IPRSUP  .GE.  1 )                                             M23.1222
     1  WRITE (LUNIT6, 156 )  K, SF3, SF6                               M37.2761
 156  FORMAT(10X,54HAPPROXIMATE CHARACTERISTIC FOR THE Q-AXIS, MACHINE NM37.2762
     1O.,I5,/,14X,6HQSAT12,14X,6HQSAT10,/,2(6X,E14.5) )                 M23.1225
      GO  TO 158                                                        M37.2763
C     KNOWN  PARAMETERS , CALCULATE CONSTANTS **************************M23.1227
C     CHECK FOR MISSING  SATURATION DATA FOR THE Q-AXIS ****************M23.1228
 157  IF( QAB .GE. QAA  .AND.  QAC .GT. QAB )  GO  TO  166              M33. 381
      KILL = 211                                                        M23.1230
      LSTAT( 19 ) = 157                                                 M23.1231
      LSTAT( 14 ) = K                                                   M23.1232
      FLSTAT( 13 ) = QAA                                                M33. 382
      FLSTAT( 14 ) = QAB                                                M23.1233
      FLSTAT( 15 ) = QAC                                                M23.1234
      GO TO 9999                                                                
 166  IF( AQ1 .LE. 0.0 )   AQ1 = 1.0                                            
      IF( AQ2 .LE. 0.0 )   AQ2 = 1.2                                            
      SF5 = QAB / ( QAA * AQ1 )                                         M38.1487
      SF2 = QAC / ( QAA * AQ2 )                                         M38.1488
      SF3 = ( SF2 - SF5 ) / ( QAC - QAB )                               M23.1238
      SF6 = QAC + ( 1.0 - SF2 ) / SF3                                   M38.1489
      IF( SF6 .GT. ( QAA * ONEHAF ) )  GO  TO  439                              
      DSM = RKV / QAA                                                   M39.1305
      HSP = RKV * ( AQ2 - AQ1 ) / ( QAC - QAB )                         M39.1306
      DSD = RKV * AQ1 - HSP * QAB                                       M39.1307
      DSR = DSD / ( DSM - HSP )                                         M39.1308
      ILV = 166                                                         M39.1309
      WRITE( LUNIT6, 6110 )   ILV, SF6, DSR                             M39.1310
      SF6 = DSR                                                         M39.1311
      SF3 = ( 1.0 - SF2 ) / ( SF6 - DAC )                               M39.1312
 439  ELP( I26+23 ) = SF6                                                       
      ELP( I26+24 ) = SF3                                                       
      IF( IPRSUP .GE. 1 )                                               M23.1242
     1WRITE(LUNIT6,156) K,SF3,SF6,SF5,SF2                               M23.1243
C     PROCESS MECHANICAL DATA OF THE GENERATOR  *********************** M38.1490
C     SCALE THE MECHANICAL DATA TO MKS UNITS   *************************M37.2767
C     THIS SCALING OF DATA CORRESPONDS TO TORQUE GIVEN IN MVA  *********M37.2768
 158  SMEXT = 0.                                                        M37.2769
      NUM2 = NUMASK + NUMASK                                            M38.1491
      NUM3 = NUM2 + NUMASK                                              M38.1492
      NUM4 = NUM2 + NUM2                                                M38.1493
      NUM5 = NUM3 + NUM2                                                M38.1494
      IB2 = ILN + NUM2                                                  M38.1495
      DO 40 I = 1, NUMASK                                               M38.1496
C     READ INPUT CARD USING CIMAGE                                      M20.1606
      CALL CIMAGE                                                       M20.1607
      IF ( KOLBEG  .GT.  0 )   GO TO 7144                               M20.1608
      DECODE(80, 36, ABUFF(1))  MLOC, EXTRS, HICO, DSR, DSM, HSP, DSD   M37.2770
  36  FORMAT( I2, 8X, 6E10.6 )                                          M37.2771
      GO TO 7145                                                        M20.1610
 7144 CALL FREFLD ( VOLTBC(1) )                                         M31. 890
      IF ( KILL  .GT.  0 )   GO TO 9999                                 M22.3106
      MLOC = VOLTBC(1)                                                  M31. 891
      NFRFLD = 1                                                        M37.2772
      CALL FREONE( EXTRS )                                              M37.2773
      CALL FREONE( HICO )                                               M37.2774
      CALL FREONE( DSR )                                                M37.2775
      CALL FREONE( DSM )                                                M37.2776
      CALL FREONE( HSP )                                                M37.2777
      CALL FREONE( DSD )                                                M37.2778
 7145 IF( MLOC .GT. 0   .AND.   MLOC .LE. NUMASK )  GO  TO  7146        M33. 386
      N167 = 7145                                                       M33. 387
 7147 KILL = 181                                                        M33. 388
      LSTAT( 19 ) = N167                                                M33. 389
      LSTAT( 18 ) = NCHAIN                                              M33. 390
      LSTAT( 15 ) = MLOC                                                M33. 391
      LSTAT( 16 ) = NUMASK                                              M33. 392
      GO  TO  9999                                                      M33. 393
 7146 IF ( NOUTPR  .EQ.  0 )                                            M37.2779
     1 WRITE( KUNIT6, 3653 ) I, MLOC, EXTRS, HICO, DSD                  M37.2780
 3653 FORMAT( 12H+  MASS CARD,   I2,   I2,   3E11.3)                    M20.1627
      IL1 = IB2 + MLOC                                                  M38.1497
      SHP(  IL1  ) = EXTRS                                              M38.1498
      SHP( IL1+NUMASK ) = HICO * BIN                                    M38.1499
      SHP( IL1+NUM2 ) = DSM * BDAM                                      M38.1500
      SHP( IL1+NUM3 ) = HSP * BDAM / TENM6                              M38.1501
      SHP( IL1+NUM4 ) = DSD * BDAM                                      M38.1502
      SHP( IL1+NUM5 ) = DSR * BDAM                                      M38.1503
 40   SMEXT = SMEXT + EXTRS                                             M37.2788
      IF(  NUMASK  .EQ.  1  )    SMEXT = 1.0                            M38.1504
      IF( ABSZ(SMEXT) .GT. FLZERO )   GO TO 410                         M38.1505
 420  KILL = 107                                                        M38.1506
      LSTAT( 19 ) = 420                                                 M38.1507
      GO TO 9999                                                        M22.3109
 410  IL2 = IB2 + NUMASK                                                M38.1508
      IL1 = IB2 + 1                                                     M38.1509
      DO 41 IL = IL1, IL2                                               M37.2791
 41   SHP( IL ) = SHP( IL ) / SMEXT                                             
C     CHECK FOR BLANK CARD TERMINATING MASS CARDS  *********************M33. 400
C     READ INPUT CARD USING CIMAGE   ***********************************M33. 401
      CALL CIMAGE                                                       M33. 402
      DECODE( 80, 1122, ABUFF(1) )   ( VOLTBC(I), I = 1, 41 )           M35. 971
 1122 FORMAT ( 41F1.0 )                                                 M35. 972
      N167 = 3654                                                       M33. 405
      IF( KILL .GT. 0 )  GO  TO  9999                                   M33. 410
C     SET-UP DUMMY BRANCHES TO RESERVE SPACE FOR FUTURE USE  ***********M33. 411
      IF ( IT+6  .LE.  LDATA )   GO TO 8917                             M20.1649
      KILL = 1                                                          M20.1650
      LSTAT(19) = 8917                                                  M20.1651
      LSTAT(16) = 3                                                     M20.1652
      GO TO 9999                                                        M22.3110
 8917 IF ( IBR+3  .LE.  LBRNCH )   GO TO 8921                           M20.1654
      KILL = 1                                                          M20.1655
      LSTAT(19) = 8921                                                  M20.1656
      LSTAT(16) = 2                                                     M20.1657
      GO TO 9999                                                        M22.3111
 8921 DO 8926  IL=1, 3                                                  M20.1659
      IT = IT + 1                                                       M20.1660
      IBR = IBR + 1                                                     M20.1661
      LENGTH( IBR ) = 1                                                 M38.1511
      KBUS( IBR ) = 1                                                   M38.1512
      MBUS( IBR ) = 1                                                   M38.1513
      NR( IBR ) = -IT                                                   M38.1514
      TR( IT ) = 1.0                                                    M38.1515
      TX( IT ) = 0.0                                                    M38.1516
 8926 C( IT ) = 0.0                                                     M38.1517
      IT = IT + 3                                                               
C     READ OUTPUT SPECIFICATION CARD. ISMOUT IS ARRAY OF OUTPUT FLAGS.  M20.1670
C     ZERO-OUT OUTPUT REQUEST ARRAYS. PRESET FLAGS FOR PARAMETER OUTPUT M33. 412
C     REQUESTS     *****************************************************M33. 413
      IEXC = 0                                                          M33. 414
      IF( ISMDAT( I30+13 ) .EQ. 0 )    IEXC = 1                                 
      N14 = I30 + 20                                                    M37.2794
      N8 = N14 - 3                                                      M33. 417
      DO 430 IL = N8, N14                                               M33. 418
 430  ISMDAT( IL ) = 0                                                  M37.2795
      ELP( I26+26 ) = 0.0                                                       
      ELP( I26+27 ) = 0.0                                                       
      ISM = 0                                                           M33. 426
      N8 = N8 - 1                                                       M33. 427
      DO 1500  IJ = 1, 41                                               M35. 974
      IF ( VOLTBC( IJ )  .NE.  0.0 )  GO TO 1983                        M35. 975
 1500 CONTINUE                                                          M35. 976
      IF ( NOUTPR .EQ. 0 )  WRITE (KUNIT6, 1700)                        M35. 978
 1700 FORMAT( 36H+ BLANK CARD TERMINATING MASS CARDS.)                  M35. 979
C     READ INPUT CARD USING CIMAGE                                      M33. 429
 7219 CALL CIMAGE                                                       M35. 980
      DECODE( 80, 3654, ABUFF(1) ) N7                                   M33. 431
 3654 FORMAT ( I8 )                                                     M35. 981
      IF( N7 .NE. 0 )   GO  TO  8235                                    M39.1315
      IF( NOUTPR .EQ. 0 )                                               M33. 433
     1 WRITE (KUNIT6, 3656)                                             M35. 982
 3656 FORMAT( 41H+ BLANK CARD TERMINATING OUTPUT REQUESTS.)             M33. 435
      GO TO 7251                                                        M33. 436
C     CHECK FOR OVERFLOW OF NPBUF() *********************************** M39.1316
 8235 N55 = ( NN4-1 ) * NUMASK + NN10                                           
      IF( N55 .LT. N56 )  GO TO  7220                                           
      KILL = 111                                                        M39.1319
      LSTAT( 13 ) = N56                                                 M39.1320
      LSTAT( 15 ) = K                                                   M39.1321
      LSTAT( 16 ) = N55                                                 M39.1322
      LSTAT( 17 ) = 0                                                   M39.1323
      LSTAT( 19 ) = 8235                                                M39.1324
      GO TO 9999                                                        M39.1325
 7220 CONTINUE                                                          M39.1326
      DECODE(80,3657,ABUFF(1) ) ( VOLTBC(I), I = 1, NN14 )              M33. 438
 3657 FORMAT(2X, 2F1.0, 4X, 12F6.0)                                     M33. 439
      IOUTR = VOLTBC( 1 )                                               M33. 440
      IALL = VOLTBC( 2 )                                                M33. 441
      IF( IOUTR .GT. 0  .AND.  IOUTR .LE. (NN4+1) )  GO  TO  7221       M33. 442
      WRITE(LUNIT6,3658) IOUTR                                          M33. 443
 3658 FORMAT( 1X, 10(1H*),  11H  WARNING  , 10( 1H* ), 3X,              M34. 111
     1 38H REQUEST FOR NONEXISTENT OUTPUT CLASS ,                       M34. 112
     2 I4, 12H  NEGLECTED. )                                            M34. 113
      GO TO 7219                                                        M33. 446
 7221 IF( NOUTPR .EQ. 0 )                                               M33. 447
     1 WRITE (KUNIT6, 3659)  IOUTR                                      M35. 983
 3659 FORMAT( 1H+, 2X, 30H OUTPUT REQUEST CARD FOR CLASS , I4 )         M34. 114
 7222 GO TO ( 7230, 7240, 7240, 7250, 7210 ), IOUTR                     M33. 450
C     PROCESS FLAGS FOR OUTPUTS AFTER THE S.S. SOLUTION  ***************M33. 451
 7210 N16 = 2                                                           M33. 452
      IVAR = 0                                                          M33. 453
      DO 7211 IPL = 3, NN14                                             M33. 454
      N6 = VOLTBC( IPL )                                                M33. 455
      IF( IALL .EQ. 0 )  GO TO 7213                                     M33. 456
      N6 = IPL - 2                                                      M33. 457
      GO TO 7212                                                        M33. 458
 7213 IF( N6 .GT. -1  .AND.  N6 .LE. N16 )  GO  TO  7212                M33. 459
      WRITE(LUNIT6,3662) N6, IOUTR, K, N16                              M33. 460
      GO TO 7211                                                        M33. 461
 7212 IF( N6 .EQ. 0 )   GO   TO  7211                                   M33. 462
      IVAR = IVAR + 1                                                   M33. 463
      IF( IVAR .GT. N16 )  GO  TO  7219                                 M33. 464
      KFLAG = I26 + N6                                                  M38.1521
      ELP( KFLAG+25 ) = N6                                              M38.1522
 7211 CONTINUE                                                          M33. 467
      GO  TO   7219                                                     M33. 468
 7230 N16 = NN10 - IEXC                                                 M33. 469
      N21 = 0                                                           M33. 470
 7231 N17 = N8 + IOUTR                                                  M33. 471
      N21 = ISMDAT( N17 ) + N21                                         M37.2798
      IF(  IALL .GT. 0 )  GO  TO  7238                                  M33. 473
      DO 7232 IL = 3, NN14                                              M33. 474
      N6 = VOLTBC( IL )                                                 M33. 475
      IF( N6 .LE. N16  .AND.  N6 .GT. -1 )  GO TO 7233                  M33. 476
      WRITE(LUNIT6,3662) N6,IOUTR,K,N16                                 M33. 477
 3662 FORMAT(1X, 10(1H*),11H  WARNING  , 10(1H*), 3X,                   M34. 115
     1 33H REQUEST FOR NONEXISTENT VARIABLE, I4, 9H IN CLASS, I3,       M34. 116
     1 13H OF S.M. NO. , I3, 12H DISCARDED. , /, 14H IN THIS CLASS,     M34. 117
     2 51H THE ABOVE S.M. CAN HAVE NUMBERS BETWEEN 1(ONE) AND, I4 )     M34. 118
      GO TO 7232                                                        M33. 482
 7233 IF( N6 .EQ. 0 )  GO TO 7232                                       M33. 483
      ISM = ISM + 1                                                     M33. 484
      N21 = N21 + 1                                                     M33. 485
      ISMDAT( N17 ) = ISMDAT( N17 ) + 1                                 M37.2799
      NPBUF( N21 ) = N6                                                 M33. 487
C     REMOVE DUPLICATE REQUSTS. PUT ALL REQUEST IN AN ASCENDING ORDER   M33. 488
      IPL = ISMDAT( N17 )  -  1                                         M37.2800
      IF( IPL .LT. 1 )  GO TO 7232                                      M33. 490
      IJN = N21                                                         M33. 491
      DO 7242 IJK = 1, IPL                                              M33. 492
      IJL = IJN - 1                                                     M33. 493
      IF( NPBUF(IJL) .LT. NPBUF(IJN) ) GO TO 7232                       M33. 494
      IF( NPBUF(IJL) .EQ. NPBUF(IJN) ) GO TO 7245                       M33. 495
      N30 = NPBUF( IJL )                                                M33. 496
      NPBUF( IJL ) = NPBUF( IJN )                                       M33. 497
      NPBUF( IJN ) = N30                                                M33. 498
 7242 IJN = IJN - 1                                                     M33. 499
      GO TO 7232                                                        M33. 500
C     DISCARD DUPLICATE REQUEST ****************************************M33. 501
 7245 WRITE(LUNIT6, 7223 ) NPBUF(IJN), IOUTR                            M33. 502
 7223 FORMAT( 1X,10(1H*), 11H  WARNING  , 10(1H*), 3X,                  M34. 119
     1 18H DUPLICATE REQUEST, I5, 10H  IN CLASS, I5, 12H  DISCARDED.)   M34. 120
      ISM = ISM - 1                                                     M33. 505
      ISMDAT( N17 ) = ISMDAT( N17 ) - 1                                 M37.2801
      DO 7246 IJK = IJN, IPL                                            M33. 507
 7246 NPBUF( IJK ) = NPBUF( IJK+1 )                                     M33. 508
 7232 CONTINUE                                                          M33. 509
 7234 IF( (IPOUT+ISM) .LE.  LSMOUT ) GO TO 7219                         M39.1327
      KILL = 111                                                        M33. 511
      LSTAT( 19 ) = 7234                                                M33. 512
      LSTAT( 13 ) =  LSMOUT                                             M39.1328
      LSTAT( 15 ) =  K                                                  M33. 515
      LSTAT( 16 ) =  IPOUT + ISM                                        M33. 516
      LSTAT( 17 ) =  0                                                  M38.1523
      GO TO 9999                                                        M33. 518
 7238 IF( N16 .GT. 0 )  GO  TO  7239                                    M33. 519
      WRITE(LUNIT6,3662) N16,IOUTR,K,N16                                M33. 520
      GO TO 7234                                                        M35. 984
 7239 N21 = N21 - ISMDAT( N17 )                                         M37.2802
      ISMDAT( N17 ) = 0                                                 M37.2803
      DO 7237 IL = 1, N16                                               M33. 524
      ISMDAT( N17 ) = ISMDAT( N17 ) + 1                                 M37.2804
      ISM = ISM + 1                                                     M33. 526
      N21 = N21 + 1                                                     M33. 527
      NPBUF( N21 ) = IL                                                 M33. 528
 7237 CONTINUE                                                          M33. 529
      GO TO 7234                                                        M33. 530
 7240 N16 = NUMASK                                                      M33. 531
      N21 = NN10 + (IOUTR-2) * NUMASK                                   M39.1329
      GO TO 7231                                                        M33. 533
 7250 N16 = NUMASK - 1                                                  M33. 534
      N21 = NN10 + (IOUTR-2) * NUMASK                                   M39.1330
      GO TO 7231                                                        M33. 536
C     PROCESSING OLD OUTPUT CARDS                                       M35. 985
 1983 N21 = 0                                                           M35. 986
      N17 = N8 + 1                                                      M35. 987
      JJ3 = 31 + NUMASK                                                 M35. 988
      JKM = -1                                                          M35. 989
      IJ = 1                                                            M35. 990
 1125 IF (IJ .EQ. 13 .OR. IJ .EQ. 23 .OR. IJ .EQ. 33) GO TO 1999        M35. 991
 1130 N6 = VOLTBC(IJ)                                                   M35. 992
      IF ( N6 .GT. -1  .AND.  N6 .LE. 2 )  GO TO 1155                   M35. 993
      WRITE ( LUNIT6, 1133 )  IJ, N6                                    M35. 994
 1133 FORMAT ( 1X, 10(1H*), 11H  WARNING  , 10(1H*)  ,/,                M35. 995
     1 30H  THE OUTPUT REQUEST IN COLUMN, I4,  11H  DISCARDED,          M35. 996
     2 29H  BECAUSE OF THE WRONG NUMBER, I4  ,/,                        M35. 997
     3 40H  THE CORRECT NUMBER SHOULD BE 1 OR 2 .  )                    M35. 998
      GO TO 1899                                                        M35. 999
 1155 IF ( N6   .EQ.  0 )  GO TO 1899                                   M35.1000
      IF ( IJ   .GE.  3 )  GO TO 1255                                   M35.1001
      KFLAG = I26 + IJ                                                  M38.1524
      ELP( KFLAG+25 ) = IJ                                                      
      GO TO 1950                                                        M35.1004
 1255 IF ( IJ .NE. 3  .AND.  IJ .NE. 11 ) GO TO 1355                    M35.1005
      JKN = 0                                                           M35.1006
      IF ( IJ  .EQ.  11 )  JKN = 7                                      M35.1007
      DO 1277 JK = 1, 3                                                 M35.1008
      N21 = N21 + 1                                                     M35.1010
      ISMDAT( N17 ) = ISMDAT( N17 ) + 1                                 M37.2805
      NPBUF(N21) = JK + JKN                                             M35.1012
 1277 CONTINUE                                                          M35.1013
      IF ( IJ  .EQ.  3 )   GO TO 1950                                   M35.1014
 1299 IJ = IJ - 3                                                       M35.1015
      GO TO 1130                                                        M35.1016
 1355 N21 = N21 + 1                                                     M37.2806
      ISMDAT( N17 ) = ISMDAT( N17 ) + 1                                 M37.2807
      IF ( IJ  .GE. 13 )  GO TO 1990                                    M35.1020
      NPBUF(N21) = IJ                                                   M35.1021
      IF ( IJ  .EQ.  8 )  NPBUF(N21) = NPBUF(N21) + 3                   M35.1022
      IF (IJ .EQ. 9  .OR.  IJ .EQ. 10) NPBUF(N21) = NPBUF(N21) + 5      M35.1023
 1899 IF ( IJ .EQ. 11 .OR.  IJ .EQ. 12)  GO TO 1299                     M35.1024
      IF ( IJ  .EQ.  10 )  IJ = IJ + 2                                  M35.1025
      IF ( IJ .EQ. 7  .OR.  IJ .EQ. 8 )  IJ = IJ + 3                    M35.1026
 1950 IJ = IJ + 1                                                       M35.1027
      IF ( IJ  .GT.  JJ3 )  GO TO 7251                                  M35.1028
      GO TO 1125                                                        M35.1029
 1990 NPBUF(N21) = IJ - 12 - JKM * 10                                           
      GO TO 1950                                                        M35.1031
 1999 N17 = N17 + 1                                                     M35.1032
      JKM = JKM + 1                                                     M35.1033
      N21 = NN10 + JKM * NUMASK                                         M39.1331
      GO TO 1130                                                        M35.1035
C     LOAD OUTPUT REQUESTS INTO PERMANENT STORAGE **********************M33. 537
 7251 IDS = IPOUT + 1                                                   M33. 538
      N21 = 0                                                           M33. 539
      IP1 = 3 * IPOUT - 2                                                       
      DO 7252 IL = 1, 4                                                 M33. 540
      N17 = N8 + IL                                                     M37.2808
      N6 = ISMDAT( N17 )                                                M37.2809
      IF( N6 .EQ. 0 )  GO TO 7252                                       M33. 543
      DO 7253 I = 1, N6                                                 M33. 544
      IPOUT = IPOUT + 1                                                 M33. 545
      N21 = N21  + 1                                                    M33. 546
      IP1 = IP1 + 3                                                             
 7253 ISMOUT( IP1 ) = NPBUF( N21 )                                              
 7252 N21 = (IL-1) * NUMASK + NN10                                      M39.1332
      IF( IDS .GT. IPOUT ) ISMDAT( N8+1 ) = -10                         M37.2810
      N2 = NTOTAC                                                       M20.1823
      I72 = 0                                                                   
      NIUNRS = IUTY( KIUTY+1 )                                                  
      N18 = 3 * NUMASK - 1                                                      
      N19 = 17                                                                  
      DO 204  I = 1, 9999                                               M38.1533
C     READ AUXILIARY INPUT TO TACS CARD                                 M20.1825
C     READ INPUT CARD USING CIMAGE                                      M20.1826
      CALL CIMAGE                                                       M20.1827
      IF ( KOLBEG  .GT.  0 )   GO TO 2183                               M22.3148
      DECODE( 80, 3669, ABUFF(1) )  N3,  BUS6,  BUS5,  IV               M38.1534
 3669 FORMAT ( I2,  2A6, I3  )                                          M38.1535
      GO TO 2185                                                        M22.3149
 2183 NFRFLD = 1                                                        M22.3150
      IF ( KILL  .GT.  0 )   GO TO 9999                                 M22.3151
      CALL FREONE ( D11 )                                               M29.1500
      N3 = D11                                                          M22.3153
      NFRFLD = -2                                                       M22.3154
      CALL FREONE ( D1 )                                                M29.1501
      BUS6 = TEXTA6(1)                                                  M22.3157
      BUS5 = TEXTA6(2)                                                  M22.3158
      NFRFLD = 1                                                        M38.1536
      CALL FREONE( D11 )                                                M38.1537
      IV = D11                                                          M38.1538
      NFRFLD = 0                                                        M38.1539
 2185 IF ( BUS6  .NE.  TEXT1 )   GO TO 3674                             M22.3159
      N2 = NTOTAC - N2                                                  M20.1831
      IF ( NOUTPR  .NE.  0 )   GO TO 7841                               M35.1036
      IF ( BUS3  .NE.  TEXT16 )                                         M35.1037
     1 WRITE (KUNIT6, 3671)  N2                                         M35.1038
 3671 FORMAT ( 9H+  END OF,  I3,                                        M20.1834
     1         27H  TACS INTERFACE VARIABLES.   )                       M35.1039
      IF ( BUS5  .EQ.  TEXT16 )                                         M35.1040
     1 WRITE (KUNIT6, 6743)  N2                                         M35.1041
 6743 FORMAT ( 9H+  END OF,  I3,                                        M35.1042
     1         37H  TACS INTERFACE VARIABLES.  PARALLEL  )              M35.1043
 7841 ISMDAT( I30+14 ) = N2                                             M37.2811
      GO TO 205                                                         M20.1837
 3674 IF ( N3  .NE.  71 )   GO TO 5431                                  M38.1540
      IF ( NOUTPR  .EQ.  0 )                                            M20.1839
     1 WRITE (KUNIT6, 3670)  BUS6                                       M35.1045
 3670 FORMAT (  33H+  TACS CONTROLLED EXCITATION.  ',  A6,  3H' .  )    M20.1841
      DO 3676  M = 1, KTAB                                                      
      NDX1 = ILNTAB( KLNTAB+M )                                         M38.1542
      IF ( BUS6  .EQ.  TEXVEC(NDX1) )   GO TO 3672                      M38.1543
 3676 CONTINUE                                                          M20.1844
      N3671 = 3676                                                              
      GO TO 5448                                                                
 3672 ISMDAT( I30+15 ) = M                                              M37.2812
      GO TO 204                                                         M20.1847
 5431 IF ( N3  .NE.  72 )   GO TO 5460                                  M38.1546
      IF ( NOUTPR  .EQ.  0 )                                            M20.1849
     1 WRITE (KUNIT6, 5436)   BUS6                                      M38.1547
 5436 FORMAT (39H+  TACS CONTROLLED MECHANICAL TORQUE. ', A6, 3H' .)    M37.2813
      N3671 = 5434                                                              
      IF( I72 .GT. 0 )   GO TO  5434                                            
      I72 = 1                                                                   
      ISMDAT( I30+16 ) = MFIRST                                                 
C     CHECK FOR POSSIBLE OVERFLOW ************************************* M38.1552
      NFIRST = MFIRST - NUMASK                                                  
      NSMOUT = 5 * ISMOLD                                                       
      D8 = NSMOUT * OM2 + 1.0                                                   
      NSMOUT = D8                                                               
      IF( NFIRST .GE. NSMOUT )    GO TO 5432                                    
      KILL = 1                                                          M38.1558
      LSTAT( 19 ) = 5432                                                M38.1559
      LSTAT( 16 ) = 16                                                  M38.1560
      GO TO 9999                                                        M38.1561
 5432 DO 5433 M = 1, NUMASK                                             M38.1562
      M1 = MFIRST - M                                                           
 5433 MASSEX( M1 ) = 0                                                          
      M1 = MFIRST                                                       M38.1565
      MFIRST = NFIRST                                                           
      NFIRST = M1                                                               
 5434 IF( IV .LE. 0 .OR. IV .GT. NUMASK )  GO TO 5448                           
      M1 = NFIRST - IV                                                          
      DO 5442  M = 1, KTAB                                              M38.1570
      NDX1 = ILNTAB( KLNTAB+M )                                         M38.1571
      IF ( BUS6  .EQ.  TEXVEC(NDX1) )   GO TO 5455                      M38.1572
 5442 CONTINUE                                                          M20.1855
      N3671 = 5442                                                              
 5448 KILL = 190                                                        M20.1856
      LSTAT( 19 ) = N3671                                               M38.1574
      LSTAT( 14 ) = N3                                                  M38.1575
      LSTAT( 15 ) = IV                                                  M38.1576
      GO TO 9999                                                        M22.3164
 5455 MASSEX( M1 ) = M                                                  M38.1577
      GO TO 204                                                                 
 5460 IF ( NOUTPR  .EQ.  0 )                                            M38.1579
     1 WRITE (KUNIT6, 3675)  N3, BUS6, IV                               M38.1580
 3675 FORMAT ( 32H+  AUXILIARY S.M. INPUT TO TACS.,  I3,                M38.1581
     1 2H ',  A6,  1H', I3 )                                            M38.1582
      LSTAT( 17 ) = 1                                                   M38.1583
      LSTAT( 19 ) = 5461                                                M38.1584
      IF( IV .GT. 0  )    GO  TO  5461                                  M38.1585
      GO  TO  202                                                               
 5461 IF( N3 .NE. 74  .AND.  N3 .NE. 73 )  GO TO  202                   M38.1587
C     LOAD ADDRESS OF BUS6 INTO TACS ARRAY 'UD1' ********************** M38.1588
      NDY5 = KUD1                                                               
      DO 5481 IP = NIUNRS, NIU                                                  
      NDX1 = ILNTAB( KALIU+IP )                                                 
      M1 = IUTY( KIUTY+IP )                                                     
      IF( M1 .NE. 92 )   GO TO 5481                                             
      IF( BUS6 .NE. TEXVEC( NDX1 ) )  GO TO 5481                                
      GO  TO  5482                                                              
 5481 NDY5 = NDY5 + 5                                                           
      LSTAT( 17 ) = -1                                                  M38.1597
      LSTAT( 19 ) = 5481                                                M38.1598
      GO TO 202                                                                 
 5482 UD1( NDY5+2 ) = NTOTAC + 1                                                
 5462 IF( N3 .EQ. 74 )    GO  TO  5463                                          
      LSTAT( 19 ) = 5462                                                        
      IF( IV .GT. N19 )   GO  TO  202                                           
      IF( ISMDAT( I30+13 ) .NE. 0  )   GO  TO  5463                     M38.1604
      IF( IV .EQ. 15 )    GO  TO  202                                           
      IV = -IV                                                                  
      GO  TO  5464                                                              
 5463 LSTAT( 19 ) = 5463                                                        
      IF( IV .GT. N18 )   GO  TO  202                                           
 5464 NTOTAC = NTOTAC + 1                                                       
      LSTAT( 19 ) = 5464                                                M38.1611
      IF( NTOTAC  .GT.  LBSTAC )  GO  TO  202                                   
      ISMTAC( NTOTAC ) = IV                                                     
      IF ( IPRSUP  .GE.  2 )                                            M20.1870
     1 WRITE( LUNIT6, 3678 )    NTOTAC,                                 M38.1614
     2  ( ISMTAC( N6 ), N6 = 1, NTOTAC )                                M38.1615
 3678 FORMAT ( /,   9H NTOTAC =,  I4,                                   M20.1873
     1  49H .   (ISMTAC(J), J = 1, NTOTAC) FOLLOWS.              ,/,    M38.1616
     2 (  12( 1X, I5 ) ) )                                              M38.1617
      GO TO 204                                                                 
  202 KILL = 108                                                        M20.1878
      LSTAT( 13 ) = IABS( IV )                                          M38.1619
      LSTAT( 14 ) = N3                                                  M38.1620
      LSTAT( 15 ) = LBSTAC                                              M38.1621
      LSTAT( 16 ) = NTOTAC                                              M38.1622
      GO TO 9999                                                        M22.3165
  204 CONTINUE                                                          M20.1884
  205 IF ( BUS5  .NE.  TEXT16 )   GO TO 99                              M20.1885
      ISM=0                                                             M20.1888
      FM=-2.0                                                           M20.1889
      GO TO 1                                                           M20.1890
   99 IF ( IPRSUP  .LE.  0 )   GO TO 4793                               M20.1891
      WRITE (LUNIT6, 4703)  NUMSM, KCONST, RMVA, RKV, AGLIN             M20.1892
 4703 FORMAT ( //,  36H DUMP OF TABLES AT END OF  'SMDAT' .,            M20.1893
     1 16H   NUMSM  KCONST,  11X,  4HRMVA,  12X,  3HRKV,                M20.1894
     2 10X,  5HAGLIN,  /,   36X,  2I8,  3E15.7  )                       M20.1895
      DO 8259 K = 1, NST                                                        
      K1 = 30 * K                                                       M38.1624
      K2 = 101 * K                                                      M38.1625
      WRITE( LUNIT6, 4707 )    K, ISMDAT( K1-29 ), ISMDAT( K1-28 ),     M38.1626
     1 ISMDAT( K1-18 ), ISMDAT( K1-17 ), ISMDAT( K1-16 ),               M38.1627
     2 ELP( K2-75 ), ELP( K2-100 ), ELP( K2-92 ), ELP( K2-98 ),         M38.1628
     3 ELP( K2-96 ), ELP( K2-97 )                                       M38.1629
 4707 FORMAT ( /,  4H ROW,                                              M20.1902
     1 48H  IMDUAL  ICONFG   NUMAS    KMAC    KEXC     CNP,             M33. 551
     2 12X,  3HELD,  12X,  3HELQ,  12X,  3HELF,  10X,  5HELFKD,         M20.1904
     3 10X, 5HELAKD, /, ( 1X, I3, 5I8, F8.0, 5E15.6) )                  M33. 552
       WRITE( LUNIT6, 4712 )   K, ELP( K2-90 ), ELP( K2-88 ),           M38.1630
     1 ELP( K2-84 ), ELP( K2-91 ), ELP( K2-89 ), ELP( K2-87 ),          M38.1631
     2 ELP( K2-95 ), ELP( K2-99 )                                       M38.1632
 4712 FORMAT ( /,  4H ROW,  12X,  3HELG,  10X,  5HELGKQ,                M20.1908
     1 12X,  3HEL0,  11X,  4HELAG,  10X,  5HELAKQ,  11X,  4HELKQ,       M38.1633
     2 11X,  4HELKD,  11X,  4HELAF ,/,  ( 1X,  I3,  8E15.6 ) )          M38.1634
      WRITE(LUNIT6, 4717)  K, ELP( K2-81 ), ELP( K2-83 ),               M38.1635
     1 ELP( K2-94 ), ELP( K2-93 ), ELP( K2-86 ), ELP( K2-85 ),          M38.1636
     2 ELP( K2-72 ), ELP( K2-71 )                                       M38.1637
 4717 FORMAT ( /,  4H ROW, 13X, 2HRA, 13X,  2HR0,  13X,  2HRF,  12X,    M38.1638
     1 3HRKD, 13X,  2HRG,  12X,  3HRKQ,  9X,  6HSMOUTP,  9X,            M38.1639
     2 6HSMOUTQ  ,/,  ( 1X,  I3,  8E15.6 )  )                           M38.1640
      N4 = K2 - 76                                                              
      WRITE( LUNIT6,4738 )  K, ISMDAT( K1-21 ), ELP(N4-3), ELP(N4-1),   M38.1642
     1  ELP(N4-2), ELP(N4), ELP(N4-4), ELP(N4-6)                        M37.2828
 4738 FORMAT(/,4H ROW,11X,4HISAT,10X,5HSAT10,9X,6HQSAT10,10X,5HSAT12,   M23.1251
     19X,6HQSAT12,11X,4HRAT1,9X,6HAGLINE,/,(1X,I3,10X,I2,3X,6E15.6))    M23.1252
 8259 CONTINUE                                                          M33. 558
      N2 = 0                                                            M37.2829
      DO 8260 K = 1, NST                                                M38.1643
      NUM2 = ISMDAT( 30*K-18 ) * 2                                              
      N3 = N2 + NUM2 * 4                                                M38.1645
      N2 = N2 + NUM2 + 1                                                        
      WRITE( LUNIT6, 4723 )   ( SHP( K2 ),  K2 = N2, N3 )               M38.1647
 4723 FORMAT( /, 43H 'SHFDAT' STORAGE CELLS FOLLOW ............, /,     M37.2836
     1 ( 1X,  8E16.7 )  )                                               M20.1919
 8260 N2 = N3 + NUM2 * 2                                                        
 4793 RETURN                                                            M20.1933
 100  CONTINUE                                                          M20.1934
C     READ PER UNIT MACHINE DATA  AS NORMALLY DEFINED IN TRANSIENT      M20.1935
C     STABILITY PROGRAMS,I.E., OBTAINED WITH AN ASSYMMETRIC MATRIX      M20.1936
      IF ( KOLBEG  .GT.  0 )   GO TO 7313                               M20.1939
      DECODE (80, 105, ABUFF(1) )                                       M20.1940
     1 ELF, ELAF, ELFKD, XD, ELAKD, ELKD                                M37.2838
      GO TO 7314                                                        M20.1942
 7313 CALL FREONE ( ELF )                                               M37.2839
      CALL FREONE ( ELAF )                                              M37.2840
      CALL FREONE ( ELFKD )                                             M37.2841
      CALL FREONE ( XD )                                                M37.2842
      CALL FREONE ( ELAKD )                                             M37.2843
      CALL FREONE ( ELKD )                                              M37.2844
 7314 IF ( NOUTPR  .EQ.  0 )                                            M20.1949
     1 WRITE (KUNIT6, 3628)  ELF, ELAF, ELFKD                           M37.2845
C     READ INPUT CARD USING CIMAGE.                                     M20.1951
      CALL CIMAGE                                                       M20.1952
      IF ( KOLBEG  .GT.  0 )   GO TO 7327                               M20.1953
      DECODE (80, 105, ABUFF(1) )                                       M20.1954
     1  ELG, ELAG, ELGKQ, XQ, ELAKQ, ELKQ                               M37.2846
  105 FORMAT(6E10.6,19X,I1)                                             M20.1956
      GO TO 7328                                                        M20.1957
 7327 CALL FREONE ( ELG )                                               M37.2847
      CALL FREONE ( ELAG )                                              M37.2848
      CALL FREONE ( ELGKQ )                                             M37.2849
      CALL FREONE ( XQ )                                                M37.2850
      CALL FREONE ( ELAKQ )                                             M37.2851
      CALL FREONE ( ELKQ )                                              M37.2852
 7328 IF ( NOUTPR  .EQ.  0 )                                            M20.1964
     1 WRITE (KUNIT6, 3637)  ELG, ELAG, ELGKQ                           M37.2853
C     READ INPUT CARD USING CIMAGE.                                     M20.1966
      CALL CIMAGE                                                       M20.1967
      IF ( KOLBEG  .GT.  0 )   GO TO 7235                               M20.1968
      DECODE (80, 110, ABUFF(1) )                                       M20.1969
     1  EL2, RA, RF, RKD, RG, RKQ, R1, EL1                              M37.2854
  110 FORMAT(8E10.6)                                                    M20.1971
      GO TO 7236                                                        M20.1972
 7235 CALL FREONE ( EL2 )                                               M29.1502
      CALL FREONE ( RA )                                                M37.2855
      CALL FREONE ( RF )                                                M37.2856
      CALL FREONE ( RKD )                                               M37.2857
      CALL FREONE ( RG )                                                M37.2858
      CALL FREONE ( RKQ )                                               M37.2859
      CALL FREONE ( R1 )                                                M29.1503
      CALL FREONE ( EL1 )                                               M29.1504
 7236 IF ( NOUTPR  .EQ.  0 )                                            M20.1981
     1 WRITE (KUNIT6, 3687)  EL2, RA, RF                                M37.2860
 3687 FORMAT ( 17H+  7TH S.M. CARD.,  3F10.4  )                         M20.1983
      ELP( I26 ) = XD * ZB                                                      
      ELP( I26+8 ) = XQ * ZB                                                    
      ELAF = ELAF * ZB                                                          
      XL = ( XD - ELAF ) * ZB                                                   
      RA = RA * ZB                                                              
      ZB3 = ZB                                                          M37.2866
C     CALCULATE THE TRANSFORMER RATIO  *********************************M20.1990
      A = ELAF / THTW                                                   M37.2867
      RAT = EMF / A                                                     M37.2868
      RAT2 = 2.0 * RAT**2 / 3.0                                         M20.1993
      RAT1 = EMF / ELAF                                                 M37.2869
      ELP( I26+1 ) = EMF                                                        
      ZB1 = ZB * RAT2                                                   M37.2871
      ELP( I26+2 ) = ELF * ZB1                                                  
      ELP( I26+4 ) = ELFKD * ZB1                                                
      ELP( I26+5 ) = ELKD * ZB1                                                 
      ELP( I26+6 ) = RF * ZB1                                                   
      ELP( I26+7 ) = RKD * ZB1                                                  
      ELP( I26+10 ) = ELG * ZB1                                                 
      ELP( I26+12 ) = ELGKQ * ZB1                                               
      ELP( I26+13 ) = ELKQ * ZB1                                                
      ELP( I26+14 ) = RG * ZB1                                                  
      ELP( I26+15 ) = RKQ * ZB1                                                 
      ZB = ZB * RAT / THTW                                              M37.2882
      ELP( I26+3 ) = ELAKD * ZB                                                 
      ELP( I26+9 ) = ELAG * ZB                                                  
      ELP( I26+11 ) = ELAKQ * ZB                                                
      ZB = ZB3                                                          M37.2886
      GO TO 70                                                          M20.2010
 9999 IF ( IPRSUP  .GE.  1 )  WRITE ( LUNIT6, 4568 )                    M35.1058
 4568 FORMAT ( 23H  "EXIT  MODULE SMDAT." )                             M35.1059
      RETURN                                                            M35.1060
      END                                                               M20.2011
      SUBROUTINE SMPFIT(X, ISM, FM, LUNIT6, NOUTPR )                    M32.2877
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS MODULE APPLIES ONLY TO  S.M.  MODELING (BOTH 50, 59).        M31. 898
C     THIS MODULE APPLIES ONLY TO S.M.  MODELLING ( BOTH 50 AND 59 )    M32.2878
      DIMENSION X(1)                                                    M32.2879
C     UNIMPROVED PARAMETERS ( RESISTANCES )    *   *   *   *   *   *   *M32.2880
      D1 = X( 1 )                                                       M32.2881
      H3 = X( 6 )                                                       M32.2882
      H2 = X( 2 )                                                       M32.2883
      IF( D1 .EQ. H2 )     H2 = H2 * FM                                 M32.2884
      A = X( 3 ) - H3                                                   M32.2885
      C = D1 - H3                                                       M32.2886
      U2 = C * C                                                        M32.2887
      IF( D1 .EQ. H2 )   GO  TO  5                                      M32.2888
      B = U2 / ( D1 - H2 )  - C                                         M32.2889
      F1 = B + C                                                        M32.2890
      F3 = F1 / X( 5 )                                                  M32.2891
      D = B * C                                                         M32.2892
      A = -A / ( ( A * F1 ) / D - 1.0 )                                 M32.2893
      F2 = A + C                                                        M32.2894
      F4 = ( A + D / F1 ) / X( 4 )                                      M32.2895
      IF( ISM  .EQ.  0  )     GO   TO   13                              M32.2896
C     IMPROVE THE ROTOR RESISTANCES    *   *   *   *   *   *   *   *   *M32.2897
      U = ( X( 4 ) + X( 5 ) ) * .5D0                                    M32.2898
      D = U * U - ( X( 4 ) * X( 5 ) ) / ( 1.0 - U2 / ( F1 * F2 ) )      M32.2899
      IF( D .LT. 0. )   GO   TO    12                                   M32.2900
      D = U - SQRTZ( D )                                                M32.2901
      F4 = F2 / D                                                       M32.2902
      F3 = F1 / ( 2.0 * U - D )                                         M32.2903
      GO  TO  13                                                        M32.2904
 12   IF( NOUTPR .NE. 0 )   GO   TO   13                                M32.2905
      WRITE (LUNIT6, 26 )                                               M32.2906
   26 FORMAT ( 5X, 36H NOTE  ----  THE LAST-READ DATA CARD,             M32.2907
     1             33H BELONGS TO A DYNAMIC SYNCHRONOUS,                M32.2908
     2             34H MACHINE, FOR WHICH THE PARAMETERS    ,/,         M32.2909
     3        18X, 33HARE TO BE MATHEMATICALLY IMPROVED,                M32.2910
     4             30H WITHIN THE MODULE "SMPFIT" OF,                   M32.2911
     5             29H OVERLAY NUMBER 5.   THE USER         ,/,         M32.2912
     6        18X, 33HREQUESTED THIS PROCEDURE BY MEANS,                M32.2913
     7             28H OF THE  "PARAMETER FITTING",                     M32.2914
     8             28H  CARD WHICH ACCOMPANIED THE       )              M35.1061
      WRITE (LUNIT6, 4267)                                              M35.1062
 4267 FORMAT (  18X,  28HDATA CARDS FOR THIS MACHINE.,                  M35.1063
     1             30H   THE ATTEMPTED ITERATION HAS,                   M35.1064
     2             20H FAILED TO CONVERGE.              )               M35.1065
      WRITE (LUNIT6, 27)                                                M32.2917
   27 FORMAT ( 18X, 31HTHE EMTP LOGIC WILL NOW RECOVER,                 M37.2887
     1              30H AND SIMPLY IGNORE THE REQUEST,                  M37.2888
     2              31H TO IMPROVE MACHINE PARAMETERS.      ,/,         M32.2920
     3         18X, 31HINPUT DATA WILL BE USED WITHOUT,                 M32.2921
     4              17H ANY ALTERATIONS.                     )          M32.2922
      GO  TO  13                                                        M32.2923
C     PARAMETERS OF THE REDUCED ORDER MODEL    *   *   *   *   *   *   *M32.2924
 5    F1 = 1.0                                                          M32.2925
      F2 = U2 / ( D1 - X( 3 ) )                                         M32.2926
      F3 = 0.                                                           M32.2927
      F4 = F2 / X( 4 )                                                  M32.2928
 13   X( 2 ) = C                                                        M32.2929
      X( 3 ) = F1                                                       M32.2930
      X( 4 ) = F2                                                       M32.2931
      X( 5 ) = F3                                                       M32.2932
      X( 6 ) = F4                                                       M32.2933
      RETURN                                                            M19. 740
      END                                                               M19. 741
