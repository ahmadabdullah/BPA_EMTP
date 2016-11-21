      SUBROUTINE OVER7 
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'SPACE2.FOR;/NOLIST'                                           
      DIMENSION LORDER(1), NDEX(1)                                      M31. 906
      EQUIVALENCE ( ICH2(1), LORDER(1) ),  ( E(1), NDEX(1) )            M31. 907
      EQUIVALENCE ( IOFKOL, IOFGND ), ( IOFKOR, IOFBND )                M33. 605
C     FOLLOWING CARRIES "NEXT" AMONG OVER6, INSERT, OVER7, & OVER9:     M37.2897
      EQUIVALENCE  ( LOOPSS(11), NEXT )                                         
      IF ( IPRSUP  .GE.  1 )                                            M32.3015
     1 WRITE (LUNIT6, 4567 )                                            M32.3016
 4567 FORMAT (23H  "BEGIN MODULE OVER7." )                              M32.3017
      ISCHM = 2                                                                 
      IF( IPRSUP .LT. 4 )  GO TO 4054                                   M42.4628
      WRITE(LUNIT6, 4050)  ISCHM, NTOT, NEXT, IOFKOL, IOFKOR, NCURR     M17. 445
 4050 FORMAT (  /,  98H SCALARS UPON ENTRY INTO RENUMBERING,  'OVER7' . M42.4629
     1    ISCHM    NTOT    NEXT  IOFKOL  IOFKOR   NCURR  ,/,            M42.4630
     2  50X, 6I8 )                                                      M42.4631
      WRITE (LUNIT6, 4053)                                              M19. 797
 4053 FORMAT (    55H INTEGER ARRAYS UPON ENTRY INTO RENUMBERING,  'OVERM42.4632
     17' .  ,/,  96H     ROW   KOLUM  KORDER   KOWNT     LOC   KODE     M32.3018
     2KBUS    MBUS      NR  LENGTH   BUS1     BUS2    )                 M32.3019
      DO 4057  I=1, NEXT                                                M19. 799
      N1 = I + IOFKOL                                                   M19. 800
      N2 = I + IOFKOR                                                   M19. 801
      NDX1 = LSWTCH + I                                                 M32.3020
      WRITE (LUNIT6, 4055) I, KOLUM(N1),KORDER(N2),KOWNT(I),LOC(I),     M19. 802
     1                     KODE(I), KBUS(I),MBUS(I),NR(I),              M32.3021
     2                     LENGTH(I), KMSWIT(I), KMSWIT(NDX1)           M32.3022
 4055 FORMAT(12I8)                                                      M19. 805
 4057 CONTINUE                                                          M19. 806
 4054 IOFFD = 0                                                         M15. 637
      TD = 0.0                                                          M42.4633
      ZZZA = 0.0                                                        M42.4634
      LASTXX = LAST                                                     M42.4635
C     N1 = LBUS + 1  // USE LBUS, NOT THIS N1,  IN FOLLOWING:           M32.3023
      CALL MOVE0 ( NDEX(1), LBUS )                                      M31. 909
      NZ = NCURR                                                        M42.4636
      I = 0                                                             M19. 807
  140 I = I + 1                                                         M19. 808
      IF (ISCHM .EQ. 3)   GO TO 1510                                    M42.4637
  150 J = KOWNT(I)                                                      M42.4638
      CALL SUBSCR ( I, LBUS, 150, 1 )                                   M31. 910
      IF( J .EQ. (-1) )  GO TO 170                                      M42.4639
  155 K =  NDEX(J+1)                                                    M42.4640
      JSUB = J + 1                                                      M31. 911
      CALL SUBSCR ( JSUB, LBUS, 155, 1 )                                M31. 912
      ICH1(I) = K                                                       M42.4641
      CALL SUBSCR ( I, LBUS, 155, 2 )                                   M31. 913
      IF ( K  .GT.  0 )   ICH2(K) = I                                   M31. 914
      CALL SUBSCR ( K, LBUS, 155, 99 )                                  M31. 915
       NDEX(J+1) = I                                                    M42.4642
      ICH2(I) = 0                                                       M42.4643
      NORDER(I) = 0                                                     M19. 809
      GO TO 180                                                         M42.4644
  170 NZ = NZ + 1                                                       M42.4645
      NORDER(I) = NZ                                                    M42.4646
      CALL SUBSCR ( I, LBUS, 170, 1 )                                   M31. 916
  180 IF ( I  .LT.  NTOT )   GO TO 140                                  M19. 810
      IF( NZ .EQ. NTOT )  GO TO 190                                     M42.4647
      LSTAT(19) = 190                                                   M42.4648
      NELIM = 0                                                         M42.4649
      GO TO 453                                                         M42.4650
  190 NCN = 0                                                           M06.  23
C     NEXT DELETE OBVIOUSLY ZERO-CELL ZEROING OF VECTORS:               M31. 917
      NELIM = 1                                                         M42.4651
  200 CONTINUE                                                          M42.4652
      IF ( NCN  .GE.  LBUS )     GO TO 220                              M29.1518
      JSUB = NCN + 1                                                    M31. 918
      CALL SUBSCR ( JSUB, LBUS, 200, 1 )                                M31. 919
      IF ( NDEX(NCN+1) .NE. 0)   GO TO 220                              M42.4653
  210 NCN = NCN +1                                                      M42.4654
      GO TO 200                                                         M42.4655
  220 IF (NCURR .LT. NELIM)   GO TO 229                                 M42.4656
      JSUB = NCN + 1                                                    M31. 920
      CALL SUBSCR ( JSUB, LBUS, 220, 1 )                                M31. 921
      I = NDEX(NCN+1)                                                   M42.4657
      INDEX(NELIM) = IOFFD +1                                           M42.4658
      CALL SUBSCR ( NELIM, LBUS, 220, 2 )                               M31. 922
      IF( IPRSUP .LE. 25 )  GO TO 222                                   M42.4659
      WRITE(LUNIT6, 4061)  NELIM, NCN, IOFFD, NTOT, NCURR, NEXT,        M22.3211
     1  MEXT, ICON                                                      M22.3212
 4061 FORMAT (   118H PREPARE TO RENUMBER NEXT NETWORK NODE IN  'OVER7' M19. 811
     1.     NELIM     NCN   IOFFD    NTOT   NCURR    NEXT    MEXT    ICOM42.4660
     2N   ,/,  54X,  8I8 )                                              M42.4661
      IF ( IPRSUP  .LE.  34 )   GO TO 222                               M19. 812
      WRITE (LUNIT6, 4062)                                              M19. 813
 4062 FORMAT (  86H RENUMBERING ARRAYS.       ROW   KOLUM  KORDER   KOWNM42.4662
     1T     LOC    ICH1    ICH2    NDEX)                                M19. 814
      DO 4066  II = 1, 15                                               M19. 815
      N1 = II + IOFKOL                                                  M19. 816
      N2 = II + IOFKOR                                                  M19. 817
      WRITE (LUNIT6, 4064) II, KOLUM(N1),KORDER(N2),KOWNT(II),LOC(II),  M19. 818
     1                     ICH1(II), ICH2(II), NDEX(II)                 M19. 819
 4064 FORMAT(22X, 8I8)                                                  M19. 820
 4066 CONTINUE                                                          M19. 821
  222 J = ICH1(I)                                                       M42.4663
      CALL SUBSCR ( I, LBUS, 222, 1 )                                   M31. 923
       NDEX(NCN+1) = J                                                  M42.4664
      JSUB = NCN + 1                                                    M31. 924
      CALL SUBSCR ( JSUB, LBUS, 222, 2 )                                M31. 925
      IF ( J  .GT.  0 )   ICH2(J) = 0                                   M31. 926
      CALL SUBSCR ( J, LBUS, 222, 99 )                                  M31. 927
      NORDER(I) = NELIM                                                 M42.4665
      GO TO 240                                                         M42.4666
  229 DO 231 I = 1, NTOT                                                M42.4667
      J = NORDER(I)                                                     M42.4668
      CALL SUBSCR ( J, LBUS, 229, 1 )                                   M31. 928
  231 LORDER(J) = I                                                     M42.4669
      IF( IPRSUP .GE. 3 )                                               M42.4670
     1 WRITE (LUNIT6, 5231)                                             M19. 822
 5231 FORMAT ( /, 122H FINAL RENUMBERING ARRAYS AT THE END OF  'OVER7' .M42.4671
     1     ROW  NORDER   INDEX    ILOC  LORDER   KOWNT     LOC   KOLUM  M42.4672
     2KORDER    )                                                       M19. 823
      DO 5235  I=1, 20                                                  M19. 824
      N1 = I + IOFKOL                                                   M19. 825
      N2 = I + IOFKOR                                                   M19. 826
      IF ( IPRSUP  .GE.  3 )                                            M19. 827
     1 WRITE (LUNIT6, 5233) I, NORDER(I), INDEX(I), ILOC(I), LORDER(I), M19. 828
     1                     KOWNT(I), LOC(I), KOLUM(N1), KORDER(N2)      M19. 829
 5233 FORMAT(50X, 9I8)                                                  M19. 830
 5235 CONTINUE                                                          M19. 831
      IF (IOFFD .LE. 0)   GO TO 233                                     M42.4673
      DO 232 I = 1, IOFFD                                               M42.4674
      CALL SUBSCR ( I, LSIZ23, 5235, 1 )                                M31. 929
      J = ILOC(I)                                                       M42.4675
      CALL SUBSCR ( J, LBUS, 5235, 2 )                                  M31. 930
      J = NORDER(J)                                                     M42.4676
  232 ILOC(I) = J                                                       M42.4677
  233 IF( NELIM .GT. NTOT )  GO TO 5236                                 M42.4678
      DO 236 I = NELIM , NTOT                                           M42.4679
      CALL SUBSCR ( I, LBUS, 233, 1 )                                   M31. 931
      INDEX(I) = IOFFD +1                                               M42.4680
      K = LORDER(I)                                                     M42.4681
      CALL SUBSCR ( K, LBUS, 233, 2 )                                   M31. 932
      J = LOC(K)                                                        M42.4682
  234 IF (J .EQ. 0)   GO TO 236                                         M42.4683
      CALL SUBSCR ( J, LSIZ23, 234, 1 )                                 M31. 933
      ISUBS1 =  IOFKOL+J                                                M16.1143
      K = KOLUM(ISUBS1)                                                 M16.1144
      CALL SUBSCR ( K, LBUS, 234, 2 )                                   M31. 934
      L = NORDER(K)                                                     M42.4684
      IF (I .GT. L)   GO TO 235                                         M42.4685
      IOFFD = IOFFD +1                                                  M42.4686
      CALL SUBSCR ( IOFFD, LSIZ23, 234, 3 )                             M31. 935
      ILOC(IOFFD) = L                                                   M42.4687
  235 ISUBS1 =  IOFKOR+J                                                M16.1145
      CALL SUBSCR ( J, LSIZ23, 235, 1 )                                 M31. 936
      J = KORDER(ISUBS1)                                                M16.1146
      GO TO 234                                                         M42.4688
  236 CONTINUE                                                          M42.4689
 5236 INDEX(NTOT+1) = IOFFD + 1                                         M42.4690
      JSUB = NTOT + 1                                                   M31. 937
      CALL SUBSCR ( JSUB, LBUS, 5236, 1 )                               M31. 938
      IF( IPRSUP .GE. 3 )                                               M42.4691
     1 WRITE(LUNIT6, 5241)  ( I, NORDER(I), INDEX(I), ILOC(I),          M22.3213
     2   I=1, 20 )                                                      M22.3214
 5241 FORMAT ( /,  65H ARRAYS UPON EXIT FROM  'OVER7' .     ROW  NORDER M42.4692
     1  INDEX    ILOC  ,/,  ( 33X, 4I8 ) )                              M42.4693
      IF ( IOFFD  .GT.  LSTAT(43) )                                     M22.3215
     1 LSTAT(43) = IOFFD                                                M22.3216
 5276 IF ( LASTOV  .GT.  NCHAIN )   GO TO 5283                          M15. 638
      IF (IPRSUP .GT. 0)                                                M32.3024
     1WRITE(LUNIT6, 47881)  ( NORDER(I), I=1, NTOT )                    M32.3025
47881 FORMAT ( /,  63H (NORDER(I), I=1, NTOT)   AFTER TRANSIENT-NETWORK M32.3026
     1RENUMBERING .    ,/,  ( 1X, 20I6 ) )                              M32.3027
      IF( IPRSUP .GE. 1 )                                               M32.3028
     1 WRITE(LUNIT6, 54230)  NEXT, LAST, NTOT, KSWTCH, KCONST, NV,      M32.3029
     2  IBR, INONL  , NPOWER, IT, ISTEAD, NORDER(1), NCURR              M32.3030
54230 FORMAT ( /,    121H BEFORE 'OVER8' .    NEXT    LAST    NTOT  KSWTM32.3031
     1CH  KCONST      NV     IBR   INONL  NPOWER      IT  ISTEAD  NOR(1)M32.3032
     2   NCURR   ,/, 17X, 13I8 )                                        M32.3033
      IF( NORDER(1) .EQ. 1 )  GO TO 14                                  M32.3034
      IF ( NOUTPR  .EQ.  0 )                                            M32.3035
     1 WRITE(LUNIT6, 47881 )  ( NORDER(I), I=1, NTOT )                  M32.3036
      IF ( IPRSUP  .GT.  0 )                                            M42.4694
     1 WRITE(LUNIT6, 6045 )                                             M32.3038
 6045 FORMAT( 114H NOTE ---- THE PRECEDING PRINTOUT SHOWS THAT GROUND WAM32.3039
     1S NOT RENUMBERED FIRST IN THE TRANSIENT-NETWORK RENUMBERING.  ,/, M32.3040
     2 11X, 112HWE WILL SWAP THE NEW NUMBERS BETWEEN GROUND AND WHATEVERM32.3041
     3 NODE WAS RENUMBERED FIRST, IN ORDER TO GET A LEGITIMATE   ,/,    M32.3042
     4 11X,  48HNUMBERING FOR THE REST OF THE EMTP CALCULATIONS.   )    M32.3043
      DO 26  I=1, NTOT                                                  M32.3044
      IF( NORDER(I) .EQ. 1 )  GO TO 32                                  M32.3045
   26 CONTINUE                                                          M32.3046
      CALL STOPTP                                                               
   32 NORDER(I) = NORDER(1)                                             M32.3048
      NORDER(1) = 1                                                     M32.3049
   14 DO 73765  I=1, NTOT                                               M32.3050
      J = NORDER(I)                                                     M32.3051
73765 ICH1(J) = I                                                       M32.3052
      IF ( KCONST  .EQ.  0 )   GO TO 73820                              M32.3053
      N1 = 0                                                            M32.3054
      J = 1                                                             M32.3055
      DO 73780  I=2, NTOT                                               M32.3056
      K = ICH1(I)                                                       M32.3057
      IF( KODE(K) .EQ. 0 )  GO TO 73775                                 M32.3058
      DO 6051 L=1, KCONST                                               M32.3059
      IF ( IPRSUP .GE. 3 )                                              M32.3060
     1 WRITE (LUNIT6, 6049)  I, K, KODE(K), N1, J, L, NODE(L),          M32.3061
     2 TSTART(L)                                                        M32.3062
 6049 FORMAT ( 43H AT 6049 OF "OVER7".  I, K, KODE(K), ETC. =,          M32.3063
     1  7I8,  E20.10  )                                                 M32.3064
C     S.M.  SOURCE NODE MUST PRECEDE REGULAR VOLTAGE SOURCE NODES.      M32.3065
C     ALTER RENUMBERING MAP TO PRODUCE THIS, IF  TSTART  .EQ.  -9988.   M32.3066
      IF ( TSTART(L)  .NE.  -9988. )   GO TO 6051                       M32.3067
      IF ( NODE(L) .EQ. K ) GO TO 73775                                 M32.3068
 6051 CONTINUE                                                          M32.3069
 2318 N1 = N1 + 1                                                       M32.3070
      NORDER(N1) = K                                                    M32.3071
      GO TO 73780                                                       M32.3072
73775 J = J + 1                                                         M32.3073
      ICH1(J) = K                                                       M32.3074
73780 CONTINUE                                                          M32.3075
      KPARTB = J                                                        M32.3076
      IF( N1 .LE. 0 )  GO TO 73820                                      M32.3077
      DO 73785  I=1, N1                                                 M32.3078
      J = J + 1                                                         M32.3079
      K = NORDER(I)                                                     M32.3080
73785 ICH1(J)=K                                                         M32.3081
      DO 73790  I=1, NTOT                                               M32.3082
      J=ICH1(I)                                                         M32.3083
73790 NORDER(J) = I                                                     M32.3084
73820 CONTINUE                                                          M32.3085
      LASTOV = NCHAIN                                                   M15. 639
      NCHAIN = NCHAIN + 1                                               M15. 640
      GO TO 5294                                                        M15. 641
 5283 N1 = NCHAIN                                                       M15. 642
      NCHAIN = LASTOV + 1                                               M15. 643
      LASTOV = N1                                                       M15. 644
      IF ( IPRSUP  .GE.  1 )                                            M32.3086
     1 WRITE (LUNIT6, 4568 )                                            M32.3087
 4568 FORMAT (23H  "EXIT  MODULE OVER7." )                              M32.3088
 5294 GO TO 99999                                                       M15. 645
  240 NELIM = NELIM +1                                                  M42.4695
      IF (ISCHM .EQ. 1)   GO TO 200                                     M42.4696
      CALL SUBSCR ( I, LBUS, 240, 1 )                                   M31. 939
      IF (LOC(I) .EQ. 0)   GO TO 200                                    M31. 940
  260 IST = LOC(I)                                                      M42.4697
      CALL SUBSCR ( I, LBUS, 260, 1 )                                   M31. 941
      JST = IST                                                         M42.4698
      GO TO 290                                                         M42.4699
  270 ISUBS1 =  IOFKOR+JST                                              M16.1147
      CALL SUBSCR ( JST, LSIZ23, 270, 1 )                               M31. 942
      JST = KORDER(ISUBS1)                                              M16.1148
  280 IF (JST .NE. 0)   GO TO 290                                       M42.4700
      IF (ISCHM .EQ. 3)   GO TO 1110                                    M42.4701
      GO TO 200                                                         M42.4702
  290 IB = IST                                                          M42.4703
      CALL SUBSCR ( JST, LSIZ23, 290, 1 )                               M31. 943
      ISUBS1 =  IOFKOL+JST                                              M16.1149
      J = KOLUM(ISUBS1)                                                 M16.1150
      CALL SUBSCR ( J, LBUS, 290, 2 )                                   M31. 944
      JB = LOC(J)                                                       M42.4704
      ICON = -1                                                         M42.4705
      JBS = 0                                                           M42.4706
  300 IF (JB .EQ. 0)   GO TO 200                                        M42.4707
      IOFFD = IOFFD +1                                                  M42.4708
      CALL SUBSCR ( IOFFD, LSIZ23, 300, 1 )                             M31. 945
      ILOC(IOFFD) = J                                                   M42.4709
  310 ISUBS1 =  IOFKOL+IB                                               M16.1151
      ISUBS2 =  IOFKOL+JB                                               M16.1152
      CALL SUBSCR ( IB, LSIZ23, 310, 1 )                                M31. 946
      CALL SUBSCR ( JB, LSIZ23, 310, 2 )                                M31. 947
      N13 = KOLUM(ISUBS1) - KOLUM(ISUBS2)                               M31. 948
      IF ( N13  .GT.  0 )   GO TO 500                                   M31. 949
      IF ( N13  .LT.  0 )   GO TO 360                                   M31. 950
C     IF (KOLUM(ISUBS1)    - KOLUM(ISUBS2)   ) 360, 320, 500            M31. 951
C 320 ISUBS1 =  IOFKOR+IB                                               M31. 952
      ISUBS1 = IOFKOR + IB                                              M31. 953
      IF (KORDER(ISUBS1)    .EQ. 0)   GO TO 430                         M16.1155
  330 ISUBS1 =  IOFKOR+IB                                               M16.1156
      CALL SUBSCR ( IB, LSIZ23, 330, 1 )                                M31. 954
      IB = KORDER(ISUBS1)                                               M16.1157
  340 ISUBS1 =  IOFKOR+JB                                               M16.1158
      CALL SUBSCR ( JB, LSIZ23, 340, 1 )                                M31. 955
      IF (KORDER(ISUBS1)    .EQ. 0)   GO TO 700                         M16.1159
  350 JBS = JB                                                          M42.4710
      ISUBS1 =  IOFKOR+JB                                               M16.1160
      CALL SUBSCR ( JB, LSIZ23, 350, 1 )                                M31. 956
      JB = KORDER(ISUBS1)                                               M16.1161
      GO TO 310                                                         M42.4711
  360 ISUBS1 =  IOFKOL+IB                                               M16.1162
      CALL SUBSCR ( IB, LSIZ23, 360, 1 )                                M31. 957
      IF (KOLUM(ISUBS1)    .EQ. J)   GO TO 410                          M16.1163
  370 ISUBS1 =  IOFKOL+NEXT                                             M16.1164
      CALL SUBSCR ( NEXT, LSIZ23, 370, 1 )                              M31. 958
      ISUBS2 =  IOFKOL+IB                                               M16.1165
      CALL SUBSCR ( IB,   LSIZ23, 370, 2 )                              M31. 959
      KOLUM(ISUBS1)      = KOLUM(ISUBS2)                                M16.1166
      MEXT = NEXT                                                       M42.4712
      IF (NEXT .EQ. LASTXX)   GO TO 375                                 M42.4713
      CALL SUBSCR ( NEXT, LSIZ23, 370, 3 )                              M31. 960
      ISUBS1 =  IOFKOR+NEXT                                             M16.1167
      NEXT = KORDER(ISUBS1)                                             M16.1168
      ISUBS1 =  IOFKOR+MEXT                                             M16.1169
      CALL SUBSCR ( MEXT, LSIZ23, 370, 4 )                              M31. 961
      KORDER(ISUBS1)      = JB                                          M16.1170
      JBT = JBS                                                         M42.4714
      JBS = MEXT                                                        M42.4715
      ICON = ICON +1                                                    M42.4716
      GO TO 380                                                         M42.4717
  375 KILL = 16                                                         M42.4718
      LSTAT(19) = 375                                                   M42.4719
      LSTAT(16)=LAST                                                    M42.4720
      LSTAT(15) = NELIM                                                 M42.4721
      IF ( IPRSUP  .GE.  1)                                             M32.3089
     1 WRITE ( LUNIT6, 4568 )                                           M32.3090
      GO TO 9999                                                        M42.4722
  380 IF (JBT .EQ. 0)   GO TO 400                                       M42.4723
  390 ISUBS1 =  IOFKOR+JBT                                              M16.1171
      CALL SUBSCR ( JBT, LSIZ23, 390, 1 )                               M31. 962
      KORDER(ISUBS1)     = MEXT                                         M16.1172
      GO TO 410                                                         M42.4724
  400 LOC(J) = MEXT                                                     M42.4725
      CALL SUBSCR ( J, LBUS, 400, 1 )                                   M31. 963
  410 ISUBS1 =  IOFKOR+IB                                               M16.1173
      CALL SUBSCR ( IB, LSIZ23, 410, 1 )                                M31. 964
      IB = KORDER(ISUBS1)                                               M16.1174
  420 IF (IB .NE. 0)   GO TO 310                                        M42.4726
  430 ISUBS1 =  IOFKOL+JB                                               M16.1175
      CALL SUBSCR ( JB, LSIZ23, 430, 1 )                                M31. 965
C     IF (KOLUM(ISUBS1)    -I) 440, 470, 790                            M31. 966
C 440 JBS = JB                                                          M31. 967
      N13 = KOLUM(ISUBS1) - I                                           M31. 968
      IF ( N13  .EQ.  0 )   GO TO 470                                   M31. 969
      IF ( N13  .GT.  0 )   GO TO 790                                   M31. 970
      JBS = JB                                                          M31. 971
      ISUBS1 =  IOFKOR+JB                                               M16.1177
      JB = KORDER(ISUBS1)                                               M16.1178
  450 IF (JB .NE. 0)   GO TO 430                                        M42.4727
      LSTAT(19) = 450                                                   M42.4728
  453 KILL = 25                                                         M42.4729
      LSTAT(16) = NELIM                                                 M42.4730
      LSTAT(15) = JBS                                                   M42.4731
      LSTAT(14) = I                                                     M42.4732
      LSTAT(13) = IB                                                    M42.4733
      LSTAT(12) = MEXT                                                  M42.4734
      LSTAT(11) = NEXT                                                  M42.4735
      IF ( IPRSUP  .GE.  1)                                             M32.3091
     1 WRITE ( LUNIT6, 4568 )                                           M32.3092
      GO TO 9999                                                        M42.4736
  470 ISUBS1 =  IOFKOR+LASTXX                                           M16.1179
      CALL SUBSCR ( LASTXX, LSIZ23, 470, 1 )                            M31. 972
      KORDER(ISUBS1)        = JB                                        M16.1180
      LASTXX = JB                                                       M42.4737
      IF (JBS .EQ. 0)   GO TO 490                                       M42.4738
  480 ISUBS1 =  IOFKOR+JBS                                              M16.1181
      ISUBS2 =  IOFKOR+JB                                               M16.1182
      CALL SUBSCR ( JBS, LSIZ23, 480, 1 )                               M31. 973
      CALL SUBSCR ( JB,  LSIZ23, 480, 2 )                               M31. 974
      KORDER(ISUBS1)     = KORDER(ISUBS2)                               M16.1183
      GO TO 790                                                         M42.4739
  490 ISUBS1 =  IOFKOR+JB                                               M16.1184
      CALL SUBSCR ( JB, LSIZ23, 490, 1 )                                M31. 975
      LOC(J) = KORDER(ISUBS1)                                           M16.1185
      CALL SUBSCR ( J,  LBUS,   490, 2 )                                M31. 976
      GO TO 790                                                         M42.4740
  500 ISUBS1 =  IOFKOL+JB                                               M16.1186
      CALL SUBSCR ( JB, LSIZ23, 500, 1 )                                M31. 977
C     IF (KOLUM(ISUBS1)    -I) 510, 540, 520                            M31. 978
C 510 JBS = JB                                                          M31. 979
      N13 = KOLUM(ISUBS1) - I                                           M31. 980
      IF ( N13  .EQ.  0 )   GO TO 540                                   M31. 981
      IF ( N13  .GT.  0 )   GO TO 520                                   M31. 982
      JBS = JB                                                          M31. 983
      ISUBS1 =  IOFKOR+JB                                               M16.1188
      JB = KORDER(ISUBS1)                                               M16.1189
      GO TO 310                                                         M42.4741
  520 ISUBS1 =  IOFKOR+JB                                               M16.1190
      CALL SUBSCR ( JB, LSIZ23, 520, 1 )                                M31. 984
      IF (KORDER(ISUBS1)    .EQ. 0)   GO TO 700                         M16.1191
  530 JBS = JB                                                          M42.4742
      ISUBS1 =  IOFKOR+JB                                               M16.1192
      CALL SUBSCR ( JB, LSIZ23, 530, 1 )                                M31. 985
      JB = KORDER(ISUBS1)                                               M16.1193
      GO TO 310                                                         M42.4743
  540 IF (JBS .EQ. 0)   GO TO 580                                       M42.4744
  550 ISUBS1 =  IOFKOR+JBS                                              M16.1194
      ISUBS2 =  IOFKOR+JB                                               M16.1195
      CALL SUBSCR ( JBS, LSIZ23, 550, 1 )                               M31. 986
      CALL SUBSCR ( JB,  LSIZ23, 550, 2 )                               M31. 987
      KORDER(ISUBS1)     = KORDER(ISUBS2)                               M16.1196
      ISUBS1 =  IOFKOR+LASTXX                                           M16.1197
      CALL SUBSCR ( LASTXX, LSIZ23, 550, 3 )                            M31. 988
      KORDER(ISUBS1)        = JB                                        M16.1198
      LASTXX = JB                                                       M42.4745
  560 ISUBS1 =  IOFKOR+JB                                               M16.1199
      CALL SUBSCR ( JB, LSIZ23, 560, 1 )                                M31. 989
      IF (KORDER(ISUBS1)    .NE. 0)   GO TO 575                         M16.1200
  570 JB = JBS                                                          M42.4746
      GO TO 700                                                         M42.4747
  575 ISUBS1 =  IOFKOR+JB                                               M16.1201
      CALL SUBSCR ( JB, LSIZ23, 575, 1 )                                M31. 990
      JB = KORDER(ISUBS1)                                               M16.1202
      GO TO 310                                                         M42.4748
  580 ISUBS1 =  IOFKOR+LASTXX                                           M16.1203
      CALL SUBSCR ( LASTXX, LSIZ23, 580, 1 )                            M31. 991
      KORDER(ISUBS1)        = JB                                        M16.1204
      LASTXX = JB                                                       M42.4749
      ISUBS1 =  IOFKOR+JB                                               M16.1205
      CALL SUBSCR ( JB, LSIZ23, 580, 2 )                                M31. 992
      IF (KORDER(ISUBS1)    .EQ. 0)   GO TO 600                         M16.1206
  590 ISUBS1 =  IOFKOR+JB                                               M16.1207
      CALL SUBSCR ( JB, LSIZ23, 590, 1 )                                M31. 993
      CALL SUBSCR ( J,  LBUS,   590, 2 )                                M31. 994
      LOC(J) = KORDER(ISUBS1)                                           M16.1208
      ISUBS1 =  IOFKOR+JB                                               M16.1209
      JB = KORDER(ISUBS1)                                               M16.1210
      GO TO 310                                                         M42.4750
  600 ISUBS1 =  IOFKOL+IB                                               M16.1211
      CALL SUBSCR ( IB, LSIZ23, 600, 1 )                                M31. 995
      IF (KOLUM(ISUBS1)    .EQ. J)   GO TO 620                          M16.1212
  610 LOC(J) = NEXT                                                     M42.4751
      CALL SUBSCR ( J, LBUS, 610, 1 )                                   M31. 996
      JB = NEXT                                                         M42.4752
      MEXT = NEXT                                                       M42.4753
C     IF (NEXT .EQ. LASTXX)   GO TO 615                                 M31. 997
      IF (NEXT .EQ. LASTXX)   GO TO 375                                 M31. 998
      ISUBS1 =  IOFKOR+NEXT                                             M16.1213
      CALL SUBSCR ( NEXT, LSIZ23, 610, 2 )                              M31. 999
      NEXT = KORDER(ISUBS1)                                             M16.1214
      ICON = ICON +1                                                    M42.4754
      ISUBS1 =  IOFKOL+JB                                               M16.1215
      ISUBS2 =  IOFKOL+IB                                               M16.1216
      CALL SUBSCR ( JB, LSIZ23, 610, 3 )                                M31.1000
      CALL SUBSCR ( IB, LSIZ23, 610, 4 )                                M31.1001
      KOLUM(ISUBS1)    = KOLUM(ISUBS2)                                  M16.1217
C     GO TO 650     ELIMINATE "CONTINUE";  USE NEXT LINE:               M31.1002
      GO TO 660                                                         M31.1003
C 615 GO TO 375    THERE WAS ONLY ONE TRANSFER TO 615                   M31.1004
  620 ISUBS1 =  IOFKOR+IB                                               M16.1218
      CALL SUBSCR ( IB, LSIZ23, 620, 1 )                                M31.1005
      IB = KORDER(ISUBS1)                                               M16.1219
  630 IF (IB .NE. 0)   GO TO 610                                        M42.4755
  640 LOC(J) = 0                                                        M42.4756
      CALL SUBSCR ( J, LBUS, 640, 1 )                                   M31.1006
      GO TO 790                                                         M42.4757
C 650 CONTINUE      ELIMINATE "CONTINUE" NOT END OF DO LOOP             M31.1007
  660 ISUBS1 =  IOFKOR+IB                                               M16.1220
      CALL SUBSCR ( IB, LSIZ23, 660, 1 )                                M31.1008
      IF (KORDER(ISUBS1)    .EQ. 0)   GO TO 690                         M16.1221
  670 ISUBS1 =  IOFKOR+IB                                               M16.1222
      CALL SUBSCR ( IB, LSIZ23, 670, 1 )                                M31.1009
      IB = KORDER(ISUBS1)                                               M16.1223
      ISUBS1 =  IOFKOL+NEXT                                             M16.1224
      ISUBS2 =  IOFKOL+IB                                               M16.1225
      CALL SUBSCR ( NEXT, LSIZ23, 670, 2 )                              M31.1010
      CALL SUBSCR ( IB,   LSIZ23, 670, 3 )                              M31.1011
      KOLUM(ISUBS1)      = KOLUM(ISUBS2)                                M16.1226
      GO TO 680                                                         M42.4758
C 675 GO TO 375        CONVERT "GO TO 675"  TO  "GO TO 375"             M31.1012
  680 MEXT = NEXT                                                       M42.4759
C     IF (NEXT .EQ. LASTXX)   GO TO 675    ELIMINATE 675 GO TO 375      M31.1013
      IF ( NEXT  .EQ.  LASTXX )   GO TO 375                             M31.1014
      ISUBS1 =  IOFKOR+NEXT                                             M16.1227
      CALL SUBSCR ( NEXT, LSIZ23, 680, 1 )                              M31.1015
      NEXT = KORDER(ISUBS1)                                             M16.1228
      ICON = ICON +1                                                    M42.4760
C     GO TO 650     ELIMINATE "650 CONTINUE";  USE NEXT LINE:           M31.1016
      GO TO 660                                                         M31.1017
  690 ISUBS1 =  IOFKOR+MEXT                                             M16.1229
      CALL SUBSCR ( MEXT, LSIZ23, 690, 1 )                              M31.1018
      KORDER(ISUBS1)      = 0                                           M16.1230
      GO TO 790                                                         M42.4761
  700 ISUBS1 =  IOFKOL+IB                                               M16.1231
      CALL SUBSCR ( IB, LSIZ23, 700, 1 )                                M31.1019
C     IF (KOLUM(ISUBS1)    -J) 710, 760, 780                            M31.1020
C 710 ISUBS1 =  IOFKOL+NEXT                                             M31.1021
      N13 = KOLUM(ISUBS1) - J                                           M31.1022
      IF ( N13  .EQ.  0 )   GO TO 760                                   M31.1023
      IF ( N13  .GT.  0 )   GO TO 780                                   M31.1024
      ISUBS1 = IOFKOL + NEXT                                            M31.1025
      CALL SUBSCR ( NEXT, LSIZ23, 700, 2 )                              M31.1026
      ISUBS2 =  IOFKOL+IB                                               M16.1234
      KOLUM(ISUBS1)      = KOLUM(ISUBS2)                                M16.1235
      ISUBS1 =  IOFKOR+JB                                               M16.1236
      CALL SUBSCR ( JB,   LSIZ23, 700, 3 )                              M31.1027
      KORDER(ISUBS1)    = NEXT                                          M16.1237
  720 MEXT = NEXT                                                       M42.4762
C     IF (NEXT .EQ. LASTXX)   GO TO 725       "725 GO TO 375"           M31.1028
      IF (NEXT .EQ. LASTXX)   GO TO 375                                 M31.1029
      ISUBS1 =  IOFKOR+NEXT                                             M16.1238
      CALL SUBSCR ( NEXT, LSIZ23, 720, 1 )                              M31.1030
      NEXT = KORDER(ISUBS1)                                             M16.1239
      ICON = ICON +1                                                    M42.4763
      ISUBS1 =  IOFKOR+IB                                               M16.1240
      CALL SUBSCR ( IB,   LSIZ23, 720, 2 )                              M31.1031
      IB = KORDER(ISUBS1)                                               M16.1241
      GO TO 730                                                         M42.4764
C 725 GO TO 375    CONVERT  "GO TO 725"  TO  "GO TO 375"                M31.1032
  730 ISUBS1 =  IOFKOL+IB                                               M16.1242
      CALL SUBSCR ( IB, LSIZ23, 730, 1 )                                M31.1033
C     IF (KOLUM(ISUBS1)    -J) 750, 650, 740    WITH 650>>>660          M31.1034
C 740 KILL = 26                                                         M31.1035
      N13 = KOLUM(ISUBS1) - J                                           M31.1036
      IF ( N13  .LT.  0 )   GO TO 750                                   M31.1037
      IF ( N13  .EQ.  0 )   GO TO 660                                   M31.1038
      KILL = 26                                                         M31.1039
      LSTAT(19) = 730                                                   M31.1040
      IF ( IPRSUP  .GE.  1 )                                            M32.3093
     1 WRITE ( LUNIT6, 4568 )                                           M32.3094
      GO TO 9999                                                        M42.4765
  750 ISUBS1 =  IOFKOL+NEXT                                             M16.1244
      ISUBS2 =  IOFKOL+IB                                               M16.1245
      CALL SUBSCR ( NEXT, LSIZ23, 750, 1 )                              M31.1041
      CALL SUBSCR ( IB,   LSIZ23, 750, 2 )                              M31.1042
      KOLUM(ISUBS1)      = KOLUM(ISUBS2)                                M16.1246
      GO TO 720                                                         M42.4766
  760 ISUBS1 =  IOFKOR+IB                                               M16.1247
      CALL SUBSCR ( IB, LSIZ23, 760, 1 )                                M31.1043
      IF (KORDER(ISUBS1)    .EQ. 0)   GO TO 790                         M16.1248
  770 ISUBS1 =  IOFKOR+IB                                               M16.1249
      CALL SUBSCR ( IB, LSIZ23, 770, 1 )                                M31.1044
      IB = KORDER(ISUBS1)                                               M16.1250
  780 ISUBS1 =  IOFKOL+NEXT                                             M16.1251
      ISUBS2 =  IOFKOL+IB                                               M16.1252
      CALL SUBSCR ( NEXT, LSIZ23, 780, 1 )                              M31.1045
      CALL SUBSCR ( IB,   LSIZ23, 780, 2 )                              M31.1046
      KOLUM(ISUBS1)      = KOLUM(ISUBS2)                                M16.1253
      ISUBS1 =  IOFKOR+JB                                               M16.1254
      CALL SUBSCR ( JB,   LSIZ23, 780, 3 )                              M31.1047
      KORDER(ISUBS1)    = NEXT                                          M16.1255
      GO TO 680                                                         M42.4767
  790 IF (ISCHM .NE. 2)   GO TO 270                                     M42.4768
      IF( KOWNT(J) .EQ. (-1) )  GO TO 270                               M42.4769
C 800 IF (ICON) 810, 270, 830                                           M31.1048
C 810 IF (KOWNT(J) +ICON .GE. NCN)   GO TO 830                          M31.1049
  800 IF ( ICON  .EQ.  0 )   GO TO 270                                  M31.1050
      CALL SUBSCR ( J, LBUS, 800, 1 )                                   M31.1051
C     THIS CHECK HERE ALSO ENSURE "J" AT S.N. 830 IS OK:                M31.1052
      IF ( ICON  .GT.  0 )   GO TO 830                                  M31.1053
      N13 = KOWNT(J) + ICON                                             M31.1054
      IF ( N13  .GE.  NCN )   GO TO 830                                 M31.1055
  820 NCN = KOWNT(J) +ICON                                              M42.4770
      CALL SUBSCR ( J, LBUS, 820, 1 )                                   M31.1056
C     CALL SUBSCR  NOT NEEDED HERE (J IS OK;  SEE BELOW 800):           M31.1057
  830 IF (ICH2(J) .EQ. 0)   GO TO 850                                   M42.4771
  840 L = ICH2(J)                                                       M42.4772
      K = ICH1(J)                                                       M42.4773
      CALL SUBSCR ( J, LBUS, 840, 1 )                                   M31.1058
      CALL SUBSCR ( K, LBUS, 840, 99 )                                  M31.1059
      CALL SUBSCR ( L, LBUS, 840, 3 )                                   M31.1060
      ICH1(L) = K                                                       M42.4774
      IF ( K  .GT.  0 )   ICH2(K) = L                                   M31.1061
      M = KOWNT(J) +ICON                                                M42.4775
      GO TO 860                                                         M42.4776
  850 K = ICH1(J)                                                       M42.4777
      CALL SUBSCR ( J, LBUS, 850, 1 )                                   M31.1062
      CALL SUBSCR ( K, LBUS, 850, 99 )                                  M31.1063
      IF ( K  .GT.  0 )   ICH2(K) = 0                                   M31.1064
      M = KOWNT(J)                                                      M42.4778
      JSUB = M + 1                                                      M31.1065
      CALL SUBSCR ( JSUB, LBUS, 850, 3 )                                M31.1066
       NDEX(M+1) = K                                                    M42.4779
      M = M +ICON                                                       M42.4780
  860 KOWNT(J) = M                                                      M42.4781
      CALL SUBSCR ( J,    LBUS, 860, 1 )                                M31.1067
      JSUB = M + 1                                                      M31.1068
      CALL SUBSCR ( JSUB, LBUS, 860, 2 )                                M31.1069
      K =  NDEX(M+1)                                                    M42.4782
       NDEX(M+1) = J                                                    M42.4783
      ICH1(J) = K                                                       M42.4784
      CALL SUBSCR ( K,    LBUS, 860, 99 )                               M31.1070
      IF ( K  .GT.  0 )   ICH2(K) = J                                   M31.1071
      ICH2(J) = 0                                                       M42.4785
      GO TO 270                                                         M42.4786
 1110 JS = LOC(I)                                                       M42.4787
      CALL SUBSCR ( I, LBUS, 1110, 1 )                                  M31.1072
      GO TO 1130                                                        M42.4788
 1120 ISUBS1 =  IOFKOR+JS                                               M16.1256
      CALL SUBSCR ( JS, LSIZ23, 1120, 1 )                               M31.1073
      JS = KORDER(ISUBS1)                                               M16.1257
 1130 ICON = 0                                                          M42.4789
 1140 IF (JS .EQ. 0)   GO TO 200                                        M42.4790
 1150 ISUBS1 =  IOFKOL+JS                                               M16.1258
      CALL SUBSCR ( JS, LSIZ23, 1150, 1 )                               M31.1074
      J = KOLUM(ISUBS1)                                                 M16.1259
      CALL SUBSCR ( J,  LBUS,   1150, 2 )                               M31.1075
      IF (KOWNT(J) .LT. 0)   GO TO 1120                                 M42.4791
      KS = LOC(J)                                                       M42.4792
      LS = KS                                                           M42.4793
      GO TO 1170                                                        M42.4794
 1160 ISUBS1 =  IOFKOR+KS                                               M16.1260
      CALL SUBSCR ( KS, LSIZ23, 1160, 1 )                               M31.1076
      KS = KORDER(ISUBS1)                                               M16.1261
 1170 IF (KS .NE. 0)   GO TO 1190                                       M42.4795
 1180 ICON = ICON /2                                                    M42.4796
      GO TO 1410                                                        M42.4797
 1190 ISUBS1 =  IOFKOL+KS                                               M16.1262
      CALL SUBSCR ( KS, LSIZ23, 1190, 1 )                               M31.1077
      K = KOLUM(ISUBS1)                                                 M16.1263
      JB = LS                                                           M42.4798
      CALL SUBSCR ( K,  LBUS,   1190, 2 )                               M31.1078
      KB = LOC(K)                                                       M42.4799
 1200 ISUBS1 =  IOFKOL+JB                                               M16.1264
      ISUBS2 =  IOFKOL+KB                                               M16.1265
C     IF (KOLUM(ISUBS1)    -KOLUM(ISUBS2)   ) 1210, 1250, 1270          M31.1079
C1210 ISUBS1 =  IOFKOL+JB                                               M31.1080
      CALL SUBSCR ( JB, LSIZ23, 1200, 1 )                               M31.1081
      CALL SUBSCR ( KB, LSIZ23, 1200, 2 )                               M31.1082
      N13 = KOLUM(ISUBS1) - KOLUM(ISUBS2)                               M31.1083
      IF ( N13  .EQ.  0 )   GO TO 1250                                  M31.1084
      IF ( N13  .GT.  0 )    GO TO 1270                                 M31.1085
      ISUBS1 = IOFKOL + JB                                              M31.1086
      IF (KOLUM(ISUBS1)    .EQ. K)   GO TO 1230                         M16.1268
 1220 ICON = ICON +1                                                    M42.4800
 1230 ISUBS1 =  IOFKOR+JB                                               M16.1269
      CALL SUBSCR ( JB, LSIZ23, 1230, 1 )                               M31.1087
      JB = KORDER(ISUBS1)                                               M16.1270
 1240 IF (JB .NE. 0)   GO TO 1200                                       M42.4801
      GO TO 1160                                                        M42.4802
 1250 ISUBS1 =  IOFKOR+JB                                               M16.1271
      CALL SUBSCR ( JB, LSIZ23, 1250, 1 )                               M31.1088
      IF (KORDER(ISUBS1)    .EQ. 0)   GO TO 1160                        M16.1272
 1260 ISUBS1 =  IOFKOR+JB                                               M16.1273
      CALL SUBSCR ( JB, LSIZ23, 1260, 1 )                               M31.1089
      JB = KORDER(ISUBS1)                                               M16.1274
 1270 ISUBS1 =  IOFKOR+KB                                               M16.1275
      CALL SUBSCR ( KB, LSIZ23, 1270, 1 )                               M31.1090
      IF (KORDER(ISUBS1)    .EQ. 0)   GO TO 1290                        M16.1276
 1280 ISUBS1 =  IOFKOR+KB                                               M16.1277
      CALL SUBSCR ( KB, LSIZ23, 1280, 1 )                               M31.1091
      KB = KORDER(ISUBS1)                                               M16.1278
      GO TO 1200                                                        M42.4803
 1290 ISUBS1 =  IOFKOL+JB                                               M16.1279
      CALL SUBSCR ( JB, LSIZ23, 1290, 1 )                               M31.1092
      IF (KOLUM(ISUBS1)    .EQ. K)   GO TO 1310                         M16.1280
 1300 ICON = ICON +1                                                    M42.4804
 1310 ISUBS1 =  IOFKOR+JB                                               M16.1281
      CALL SUBSCR ( JB, LSIZ23, 1310, 1 )                               M31.1093
      IF (KORDER(ISUBS1)    .EQ. 0)   GO TO 1160                        M16.1282
 1320 ISUBS1 =  IOFKOR+JB                                               M16.1283
      CALL SUBSCR ( JB, LSIZ23, 1320, 1 )                               M31.1094
      JB = KORDER(ISUBS1)                                               M16.1284
      GO TO 1290                                                        M42.4805
C1410 IF (KOWNT(J) -ICON) 1440, 1120, 1420                              M31.1095
C1420 IF (NCN .LE. ICON)   GO TO 1440                                   M31.1096
 1410 CALL SUBSCR ( J, LBUS, 1410, 1 )                                  M31.1097
      N13 = KOWNT(J) - ICON                                             M31.1098
      IF ( N13  .LT.  0 )   GO TO 1440                                  M31.1099
      IF ( N13  .EQ.  0 )  GO TO 1120                                   M31.1100
      IF ( NCN  .LE.  ICON )   GO TO 1440                               M31.1101
 1430 NCN = ICON                                                        M42.4806
 1440 IF (ICH2(J) .EQ. 0)   GO TO 1460                                  M42.4807
C     NO CHECK OF "J" NEEDED AT 1440 ABOVE, SINCE ONLY THRU 1410        M31.1102
C     CALL SUBSCR ( J, LBUS,  ----- NOT NEEDED.                         M31.1103
 1450 L = ICH2(J)                                                       M42.4808
      K = ICH1(J)                                                       M42.4809
      CALL SUBSCR ( J, LBUS, 1450, 1 )                                  M31.1104
      CALL SUBSCR ( L, LBUS, 1450, 2 )                                  M31.1105
      CALL SUBSCR ( K, LBUS, 1450, 3 )                                  M31.1106
      ICH1(L) = K                                                       M42.4810
      ICH2(K) = L                                                       M42.4811
      M = ICON                                                          M42.4812
      GO TO 1470                                                        M42.4813
 1460 K = ICH1(J)                                                       M42.4814
      CALL SUBSCR ( J, LBUS, 1460, 1 )                                  M31.1107
      CALL SUBSCR ( K, LBUS, 1460, 2 )                                  M31.1108
      ICH2(K) = 0                                                       M42.4815
      M = KOWNT(J)                                                      M42.4816
      JSUB = M + 1                                                      M31.1109
      CALL SUBSCR ( JSUB, LBUS, 1460, 3 )                               M31.1110
       NDEX(M+1) = K                                                    M42.4817
      M = ICON                                                          M42.4818
 1470 KOWNT(J) = M                                                      M42.4819
      JSUB = M + 1                                                      M31.1111
      CALL SUBSCR ( JSUB, LBUS, 1470, 1 )                               M31.1112
      CALL SUBSCR ( J,    LBUS, 1470, 2 )                               M31.1113
      K =  NDEX(M+1)                                                    M42.4820
       NDEX(M+1) = J                                                    M42.4821
      ICH1(J) = K                                                       M42.4822
      ICH2(K) = J                                                       M42.4823
      CALL SUBSCR ( K,    LBUS, 1470, 3 )                               M31.1114
      ICH2(J) = 0                                                       M42.4824
      GO TO 1120                                                        M42.4825
 1510 ICON = 0                                                          M42.4826
      CALL SUBSCR ( I, LBUS, 1510, 1 )                                  M31.1115
      IF (KOWNT(I) .LT. 0)   GO TO 170                                  M42.4827
      KS = LOC(I)                                                       M42.4828
      LS = KS                                                           M42.4829
 1520 IF (KS .NE. 0)   GO TO 1540                                       M42.4830
 1530 J = ICON / 2                                                      M42.4831
      CALL SUBSCR ( I, LBUS, 1530, 1 )                                  M31.1116
      KOWNT(I) = J                                                      M42.4832
      GO TO 155                                                         M42.4833
 1540 ISUBS1 =  IOFKOL+KS                                               M16.1285
      CALL SUBSCR ( KS, LSIZ23, 1540, 1 )                               M31.1117
      K = KOLUM(ISUBS1)                                                 M16.1286
      JB = LS                                                           M42.4834
      KB = LOC(K)                                                       M42.4835
      CALL SUBSCR ( K,  LBUS,   1540, 2 )                               M31.1118
 1550 ISUBS1 =  IOFKOL+JB                                               M16.1287
      ISUBS2 =  IOFKOL+KB                                               M16.1288
      CALL SUBSCR ( JB, LSIZ23, 1550, 1 )                               M31.1119
      CALL SUBSCR ( KB, LSIZ23, 1550, 2 )                               M31.1120
C     IF (KOLUM(ISUBS1)    -KOLUM(ISUBS2)   ) 1600, 1560, 1580          M31.1121
C1560 ISUBS1 =  IOFKOR+JB                                               M31.1122
      N13 = KOLUM(ISUBS1) - KOLUM(ISUBS2)                               M31.1123
      IF ( N13  .LT.  0 )   GO TO 1600                                  M31.1124
      IF ( N13  .GT.  0 )  GO TO 1580                                   M31.1125
      ISUBS1 = IOFKOR + JB                                              M31.1126
      IF (KORDER(ISUBS1)    .EQ. 0)   GO TO 1680                        M16.1291
 1570 ISUBS1 =  IOFKOR+JB                                               M16.1292
      CALL SUBSCR ( JB, LSIZ23, 1570, 1 )                               M31.1127
      JB = KORDER(ISUBS1)                                               M16.1293
 1580 ISUBS1 =  IOFKOR+KB                                               M16.1294
      CALL SUBSCR ( KB, LSIZ23, 1580, 1 )                               M31.1128
      IF (KORDER(ISUBS1)    .EQ. 0)   GO TO 1640                        M16.1295
 1590 ISUBS1 =  IOFKOR+KB                                               M16.1296
      CALL SUBSCR ( KB, LSIZ23, 1590, 1 )                               M31.1129
      KB = KORDER(ISUBS1)                                               M16.1297
      GO TO 1550                                                        M42.4836
 1600 ISUBS1 =  IOFKOL+JB                                               M16.1298
      CALL SUBSCR ( JB, LSIZ23, 1600, 1 )                               M31.1130
      IF (KOLUM(ISUBS1)    .EQ. K)   GO TO 1620                         M16.1299
 1610 ICON = ICON +1                                                    M42.4837
 1620 ISUBS1 =  IOFKOR+JB                                               M16.1300
      CALL SUBSCR ( JB, LSIZ23, 1620, 1 )                               M31.1131
      JB = KORDER(ISUBS1)                                               M16.1301
 1630 IF (JB .NE. 0)   GO TO 1550                                       M42.4838
      GO TO 1680                                                        M42.4839
 1640 ISUBS1 =  IOFKOR+JB                                               M16.1302
      CALL SUBSCR ( JB, LSIZ23, 1640, 1 )                               M31.1132
      IF (KORDER(ISUBS1)    .EQ. K)   GO TO 1660                        M16.1303
 1650 ICON = ICON +1                                                    M42.4840
 1660 ISUBS1 =  IOFKOR+JB                                               M16.1304
      CALL SUBSCR ( JB, LSIZ23, 1660, 1 )                               M31.1133
      IF (KORDER(ISUBS1)    .EQ. 0)   GO TO 1680                        M16.1305
 1670 ISUBS1 =  IOFKOR+JB                                               M16.1306
      CALL SUBSCR ( JB, LSIZ23, 1670, 1 )                               M31.1134
      JB = KORDER(ISUBS1)                                               M16.1307
      GO TO 1640                                                        M42.4841
 1680 ISUBS1 =  IOFKOR+KS                                               M16.1308
      CALL SUBSCR ( KS, LSIZ23, 1680, 1 )                               M31.1135
      KS = KORDER(ISUBS1)                                               M16.1309
      GO TO 1520                                                        M42.4842
 9999 IF ( LASTOV  .GT.  NCHAIN )   GO TO 9996                          M15. 646
      KILL=0                                                            M42.4843
      WRITE (LUNIT6, 9998)                                              M19. 832
 9998 FORMAT( /, 106H WARNING ---- NODE RENUMBERING OF THE TRANSIENTS NEM42.4844
     1TWORK HAS BROKEN DOWN, PRESUMABLY DUE TO TABLE OVERFLOW  ,/,      M42.4845
     2 113H (I.E., THE NETWORK IS TOO BIG AND/OR TOO DENSE FOR THE PRESEM19. 833
     3NT EMTP DIMENSIONING).   THE NEXT TIME THAT THE USER         ,/,  M19. 834
     4 111H REDIMENSIONS THE EMTP, HE IS ADVISED TO INCREASE THE SIZE OFM19. 835
     5 LIST NUMBER  5  AND/OR  8 .   BOTH OF THESE LISTS           ,/,  M19. 836
     6 118H CONTRIBUTE FULLY (100 PER CENT) TO DEPENDENT LIST NUMBER  99M19. 837
     7 ,   WHICH IS WHAT HAS ACTUALLY OVERFLOWED AT THIS POINT.    )    M19. 838
      NELIM = NELIM - 1                                                 M19. 839
      WRITE (LUNIT6, 9997)  NELIM, NTOT, NCURR                          M19. 840
 9997 FORMAT( 1X, I4,  23H NODES OUT OF TOTAL OF  , I4,                 M42.4846
     1  61H WERE RENUMBERED BEFORE BREAKDOWN IN THE RENUMBERING OVERLAY.M42.4847
     2 ,/,  18H HAD WE MADE IT TO,  I6,  80H   NODES, THE OPERATION WOULM19. 841
     3D HAVE TERMINATED NORMALLY (SINCE THE REMAINING ONES         ,/,  M19. 842
     4  67H ARE ALWAYS FORCED LAST WITHOUT REGARD TO SPARSITY CONSIDERATM19. 843
     5IONS).     )                                                      M19. 844
      DO 2236  I=1, NTOT                                                M19. 845
      IF ( NORDER(I)  .GT.  0 )   GO TO 2236                            M19. 846
      NELIM = NELIM + 1                                                 M19. 847
      NORDER(I) = NELIM                                                 M19. 848
 2236 CONTINUE                                                          M19. 849
      WRITE (LUNIT6, 2240)                                              M19. 850
 2240 FORMAT ( /, 107H ANYWAY, THE EMTP WILL TRY TO CONTINUE WITH EXECUTM19. 851
     1ION OF THIS DATA CASE, AS BEST IT CAN.   NODES WHICH WERE      ,/,M19. 852
     2 110H NOT RENUMBERED BEFORE THE OVERFLOW LIMIT WAS REACHED WILL NOM19. 853
     3W SIMPLY BE RENUMBERED IN THEIR ORIGINAL RELATIVE            ,/,  M19. 854
     4 117H ORDER, WITHOUT REGARD TO SPARSITY CONSIDERATIONS.   RECALL TM19. 855
     5HAT THE ORIGINAL NODE ORDER COMES FROM THE ORDER OF DATA       ,/,M19. 856
     6  88H INPUT (THE ORDER IN WHICH NODE NAMES ARE ENCOUNTERED, AS THEM19. 857
     7 EMTP DATA CARDS ARE READ).        )                              M19. 858
      WRITE (LUNIT6, 2241)  ( NORDER(I), I=1, NTOT )                    M19. 859
 2241 FORMAT (  86H THE FINAL RENUMBERING MAP   (NORDER(I), I=1, NTOT)  M19. 860
     1 WILL THEN APPEAR AS FOLLOWS ....    ,/,  ( 1X, 20I6 )  )         M19. 861
      WRITE (LUNIT6, 2247)                                              M19. 862
 2247 FORMAT ( 103H THE  K-TH  SUCH ENTRY GIVES THE ROW (AND COLUMN) POSM19. 863
     1ITION OF OLD VARIABLE  K  IN THE REORDERED MATRIX.   ,/,  1X  )   M19. 864
      GO TO 5276                                                        M15. 647
 9996 LSTAT(18)=7                                                       M42.4848
      LASTOV = NCHAIN                                                   M15. 648
      NCHAIN = 51                                                       M42.4849
      IF ( IPRSUP  .GE.  1 )                                            M32.3095
     1 WRITE (LUNIT6, 4568 )                                            M32.3096
99999 RETURN                                                            M42.4850
      END                                                               M42.4851
      SUBROUTINE SUBSCR ( J, LIMIT, ISTAT, N1 )                         M31.1136
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
C     MODULE USED TO CHECK OUT-OF-BOUNDS SUBSCRIPTS FOR "OVER7"         M31.1137
C     SPECIAL N1=99 CASE IS FOR SUBSCRIPTS WHICH ARE USED ONLY          M31.1138
C     IF POSITIVE (WSM CHANGES TO WALKER'S CODE).                       M31.1139
      IF ( J  .GE.  1     .AND.                                         M31.1140
     1     J  .LE.  LIMIT )   GO TO 9000                                M31.1141
      IF ( J  .LE.  0   .AND.   N1  .EQ.  99 )   GO TO 9000             M31.1142
      WRITE (LUNIT6, 1487)  J, LIMIT, ISTAT, N1                         M32.3098
 1487 FORMAT (  26H OUT-OF-BOUNDS SUBSCRIPT =,  I6,                     M31.1144
     1          12H .   LIMIT =,  I6,                                   M31.1145
     2          15H .   BELOW S.N.,  I6,                                M31.1146
     3          20H   WITH SEQUENCE NO.,  I2,  2H .  )                  M31.1147
 9000 IF ( IPRSUP  .GE.  99 )                                           M31.1148
     1 WRITE (LUNIT6, 9006)  J, LIMIT, ISTAT, N1                        M32.3099
 9006 FORMAT (  30H TRACE.  J, LIMIT, ISTAT, N1 =,  4I8 )               M31.1150
      RETURN                                                            M31.1151
      END                                                               M31.1152
