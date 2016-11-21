      SUBROUTINE OVER8 
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'SYNMAC.FOR;/NOLIST'                                           
      INCLUDE  'UMDECK.FOR;/NOLIST'                                           
      INCLUDE  'SPACE2.FOR;/NOLIST'                                           
C %INCLUDE  '//c/tsu/cables.INS.FTN'                                    M43. 852
      COMMON / LINEMODEL / KEXACT, NSOLVE, FMINSV,NUMRUN,NPHLMT
      COMMON / LINEMODEL / CHAR80, CHLMFS(18)
      CHARACTER*6 CHLMFS     ! 9-PHASE AS LIMIT FOR LMFS TEST
      CHARACTER*80 CHAR80


      DIMENSION INFDLI(1)                                               M43. 853
      EQUIVALENCE  ( NAMEBR(1),  INFDLI(1) )                                    
      DIMENSION  WK1(1)                                                 M43. 855
      EQUIVALENCE ( SEMAUX(1), WK1(1) )                                 M43. 856
      DIMENSION  VIM(1)                                                 M31.1154
      EQUIVALENCE ( VOLT(1), VIM(1) )                                   M31.1155
      DIMENSION  ISPUM(1)                                               M32.3100
      EQUIVALENCE  ( SPUM(1), ISPUM(1) )                                M32.3101
      DIMENSION  INTEGX(1)                                              M33. 606
      EQUIVALENCE  ( X(1), INTEGX(1) ),   ( MONCAR(1), KNT )            M37.2902
      CHARACTER*6  TEMPBUS
 1001 IF ( IPRSUP  .GE.  1 )                                            M32.3102
     1 WRITE (LUNIT6, 101)  LOOPSS(1), IV, IT, TMAX                     M37.2903
  101 FORMAT ( 28H TOP OF "OVER8".   LOOPSS(1),                         M31.1158
     1         15H IV, IT, TMAX =,  3I8, E15.5 )                        M37.2904
      ISECTI = 400
      N7 = 1                                                            M32.3105
      IF ( TMAX .LE. 0.0  .AND.  NCHAIN .GT. LASTOV )   GO TO 40014     M32.3106
      N7 = 2                                                            M32.3107
      REWIND LUNIT2                                                     M34. 125
      CALL TAPSAV ( INTEGX(1), LUNIT2, IV, N7 )                         M34. 126
      N12 = 0                                                           M32.3110
      CALL VECRSV ( VOLT(1), N12, N12 )                                 M32.3111
      N12 = KTRLSW(7)                                                   M32.3112
      CALL VECRSV (  C(1), N12, N7 )                                    M32.3113
      CALL VECRSV ( TR(1), N12, N7 )                                    M32.3114
      CALL VECRSV ( TX(1), N12, N7 )                                    M32.3115
      CALL VECRSV (  R(1), N12, N7 )                                    M32.3116
      N12 = KTRLSW(8)                                                   M32.3117
      CALL VECISV (     NR(1), N12, N7 )                                M32.3118
      CALL VECISV ( LENGTH(1), N12, N7 )                                M32.3119
      IF ( NUMUM .EQ. 0 .OR. LASTOV .LT. NCHAIN ) GO TO 6123            M37.2905
      IF ( LOOPSS(1)  .EQ.  7766 )  GO TO 6121                          M37.2906
      CALL VECISV( KSSFRQ(1), NTOT, N7 )                                M37.2907
      CALL VECISV( KPSOUR(1), NTOT, N7 )                                M37.2908
      GO TO 6123                                                        M37.2909
 6121 M7 = 0                                                            M37.2910
      M8 = 8                                                            M37.2911
      CALL VECRSV( VOLT(1), M8, M7 )                                    M37.2912
 6123 IF ( KTRLSW(5) .EQ. 1  .AND.  LASTOV .GT. NCHAIN )                M37.2913
     1  CALL VECRSV (CNVHST(1), LHIST, N7)                              M32.3121
      IF ( LOOPSS(1) .NE. 7766 )  GO TO 3489                            M32.3122
C     BACK FROM "OVER11" AFTER FINAL PHASOR SOLUTION, SO EXIT:          M32.3123
      LASTOV = NCHAIN                                                   M32.3124
      NCHAIN = 12                                                       M32.3125
      LSTAT(22) = IBR                                                   M35.1083
      GO TO 9600                                                        M32.3126
C     2ND OR LATER PASS SKIPS RENUMBERING OF 3RD PARTITION LABCOM:      M32.3127
 3489 IF ( LASTOV  .GT.  NCHAIN )  GO TO 40014                          M32.3128
      IF (INONL.EQ.0) GO TO 4141                                        M32.3129
      DO 40104 I=1,INONL                                                M42.4855
      K = NONLK(I)                                                      M42.4856
      M = NONLM(I)                                                      M42.4857
      L = IABS(K)                                                       M11. 112
      NONLK(I) = NORDER(L)                                              M11. 113
      IF( K .LT. 0 )  NONLK(I) = - NONLK(I)                             M42.4858
      L = IABS(M)                                                       M11. 114
      NONLM(I) = NORDER(L)                                              M11. 115
      IF( M .LT. 0 )  NONLM(I) = - NONLM(I)                             M42.4859
40104 CONTINUE                                                          M42.4860
 4141 DO 40007 I=1,IBR                                                  M32.3130
      K = KBUS(I)                                                       M42.4861
      M = MBUS(I)                                                       M42.4862
      L = IABS(K)                                                       M11. 116
      KBUS(I) = NORDER(L)                                               M11. 117
      IF( K .LT. 0 )  KBUS(I) = - KBUS(I)                               M42.4863
      L = IABS(M)                                                       M11. 118
      MBUS(I) = NORDER(L)                                               M11. 119
      IF( M .LT. 0 )  MBUS(I) = - MBUS(I)                               M42.4864
40007 CONTINUE                                                          M42.4865
      DO 3430  I=2, NTOT                                                M42.4866
 3430 ICH2(I) = I                                                       M42.4867
      DO 3456  I=2, NTOT                                                M42.4868
 3442 L = ICH2(I)                                                       M42.4869
      J = NORDER(L)                                                     M42.4870
      IF( J .EQ. I )  GO TO 3456                                        M42.4871
      BUS1 = BUS(J)                                                     M42.4872
      NSAVE = KTRANS(J)                                                 M43. 857
      N3 = ICH2(J)                                                      M42.4873
      BUS(J) = BUS(I)                                                   M42.4874
      KTRANS(J) = KTRANS(I)                                             M43. 858
      ICH2(J) = ICH2(I)                                                 M42.4875
      BUS(I) = BUS1                                                     M42.4876
      KTRANS(I) = NSAVE                                                 M43. 859
      ICH2(I) = N3                                                      M42.4877
      GO TO 3442                                                        M42.4878
 3456 CONTINUE                                                          M42.4879
      IF( IPRSUP .GT. 1 )                                               M36. 709
     1WRITE(LUNIT6, 73825)  KPARTB, NTOT                                M32.3131
73825 FORMAT( //, 33H FINAL RENUMBERING MAPS.  KPARTB= , I5,            M32.3132
     1 5X, 5HNTOT=, I3,                                                 M32.3133
     1       /, 9X, 1HI, 4X, 6HBUS(I), 3X, 9HNORDER(I), 5X,             M42.4880
     2   7HICH1(I) , 5X, 7HKODE(I)   )                                  M32.3134
      IF( IPRSUP .GT. 1 )                                               M42.4881
     1WRITE(LUNIT6, 73826)  ( I, BUS(I), NORDER(I),   ICH1(I),          M17. 465
     1 KODE(I), I=1, NTOT )                                             M32.3135
73826 FORMAT( I10, 4X, A6, 3I12 )                                       M42.4882
      IF (KSWTCH.LT.1) GO TO 40017                                      M42.4883
      DO 40008 I=1,KSWTCH                                               M42.4884
      NDX1 = LSWTCH + I                                                 M32.3136
      K = KMSWIT(I)                                                     M32.3137
      M = KMSWIT(NDX1)                                                  M32.3138
      KMSWIT(I) = NORDER(K)                                             M32.3139
40008 KMSWIT(NDX1) = NORDER(M)                                          M32.3140
40017 IF (KCONST.LT.1) GO TO 40011                                      M42.4885
      DO 40009 I=1,KCONST                                               M42.4886
      K = NODE(I)                                                       M42.4887
      L = IABS(K)                                                       M11. 120
      NODE(I) = NORDER(L)                                               M11. 121
      IF( K .LT. 0 )  NODE(I) = - NODE(I)                               M42.4888
      IF ( IFORM(I) .NE. 18 )   GO TO 40009                             M32.3141
      N6 = TIME1(I)                                                     M32.3142
      TIME1(I) = NORDER(N6)                                             M32.3143
40009 CONTINUE                                                          M42.4889
40011 IF(NV.EQ.0) GO TO 40013                                           M31.1161
      DO 40012 I=1,NV                                                   M42.4890
      K = IBRNCH(I)                                                     M42.4891
      IBRNCH(I) = NORDER(K)                                             M42.4892
      M = JBRNCH(I)                                                     M42.4893
40012 JBRNCH(I) = NORDER(M)                                             M42.4894
40013 IF (NUMSM .EQ. 0 ) GO TO 40014                                    M16.1324
C  TRANSIENT RENUMBERING OF SYNCHRONOUS MACHINE NODES                   M16.1325
      J30 = 2                                                                   
      DO 100 I = 1, NUMSM                                                       
      DO 99 IJI = 1, 3                                                          
      J31 = J30 + IJI                                                   M38.1649
      KSMA = ISMDAT( J31 )                                                      
 99   ISMDAT( J31 ) = NORDER( KSMA )                                            
 100  J30 = J30 + 30                                                            
40014 IF (NUMUM .EQ. 0 .OR. NCHAIN .LT. LASTOV) GO TO 6666              M37.2922
      CALL FRQCHK                                                       M37.2923
      N20 = 0                                                           M37.2924
      IF (ISTART .EQ. 0 .AND. LOOPSS(10) .EQ. 0) N20 = 1                M37.2925
      IF (N20 .EQ. 1 .AND. INITUM .EQ. 0) N20 = 2                       M37.2926
      IF (NUMUM .GT. 0 .AND. N20 .EQ. 1) KILL = 0                       M37.2927
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M37.2928
      M7 = 1                                                            M37.2929
      CALL VECISV ( KSSFRQ(1), NTOT, M7 )                               M37.2930
      CALL VECISV ( KPSOUR(1), NTOT, M7 )                               M37.2931
 6666 IF ( NCHAIN  .GT.  LASTOV )   GO TO 2236                          M37.2932
      CALL VECRSV ( TCLOSE(1), KSWTCH, N7 )                             M33. 611
      N12 = -1                                                                  
      IF ( KTRLSW(5) .EQ. 1 ) N12 = -2                                          
      N15 = 0                                                                   
      CALL VECRSV( VOLT(1), N12, N15 )                                  M33. 612
C     PRECEDING CALL TO "FRQCHK" SHOWS USER-DEFINED SOURCES             M31.1165
C     IDENTIFIED BY SUBNETWORK FOR USE IN FOLLOWING "UMRENU",           M31.1166
C     FOR CASES HAVING ONE OR MORE U.M. (NUMUM .GT. 0).                 M31.1167
 2236 IF ( NUMUM  .GT.  0  )                                            M37.2933
     1 CALL UMRENU ( SPUM(IUREAC),                                      M33. 614
     1 SPUM(IUGPAR), SPUM(IUFPAR), SPUM(IUHIST), SPUM(IUUMRP),          M33. 615
     2 ISPUM(IUNOD1), ISPUM(IUNOD2), ISPUM(IUJCLT), ISPUM(IUJCLO),      M32.3145
     3 ISPUM(IUJTYP), ISPUM(IUNODO), ISPUM(IUJTMT), SPUM(IUHISM),       M32.3146
     4 SPUM(IUOMGM), SPUM(IUOMLD), SPUM(IUTHAM), SPUM(IUREDU),          M31.1173
     5 SPUM(IUREDS), SPUM(IUFLDS), SPUM(IUFLDR), SPUM(IUREQU),          M31.1174
     6 SPUM(IUFLQS), SPUM(IUFLQR), ISPUM(IUJCDS), ISPUM(IUJCQS),        M32.3147
     7 SPUM(IUFLXD), SPUM(IUFLXQ), ISPUM(IUNPPA), SPUM(IUROTM),         M32.3148
     8 ISPUM(IUNCLD), ISPUM(IUNCLQ), ISPUM(IUJTQO), ISPUM(IUJOMO),      M32.3149
     9 ISPUM(IUJTHO), SPUM(IUREQS), SPUM(IUEPSO), SPUM(IUDCOE),         M32.3150
     9 ISPUM(IUKCOI), SPUM(IUVOLT), SPUM(IUANGL), ISPUM(IUNODF),        M32.3151
     9 ISPUM(IUNODM), ISPUM(IUKUMO), ISPUM(IUJUMO), SPUM(IUUMOU))       M32.3152
      IF ( KILL .GT. 0 )  GO TO 9200                                    M32.3153
      IF ( ISTEAD  .EQ.  0 )   GO TO 9500                               M33. 616
      IF ( FMAXFS  .EQ.  0.0 )   GO TO 1413                             M32.3160
      IF ( KNT  .EQ.  1 )   GO TO 3224                                  M32.3161
      IBR = IT1                                                         M21.  60
      IF ( FMINFS  .LE.  FMAXFS )   GO TO 3234                          M20.2193
      IF ( KEXACT .NE. 88333 )  GO TO 2240
      NSOLVE = NSOLVE + 1
      IF ( NSOLVE .GT. 1  ) GO TO 2238
      FMINFS = FMINSV
      KODSEM(1) = 0     ! KBRNUM is replaced by '1', 2/21/90, THL.
      IMODEL(1) = 0
      KBUS(1) = -2
      KODEBR(1) = 0
      LENGTH(1) = NPHLMT
      KNT = 1
      LASTOV = 8
      NCHAIN = 44
      GO TO 9600 
 2238 IF ( NUMRUN .EQ. 3  )  GO TO 2240
      IF ( NUMRUN .EQ. 2  .AND. LENGTH(1) .LE. 3 )  GO TO 2240
      NSOLVE = 0
      LLBUFF = -3333
      REWIND  5
      CALL DATAIN    !  go to datain to regenerate data input for LMFS test
      LASTOV = NCHAIN
      NCHAIN = 1    ! program generated 2nd & 3rd LMFS data
      GO TO 9600
 2240 BEGMAX(1) = 0.0                                                   
      LASTOV = NCHAIN                                                   M20.2195
      NCHAIN = 20                                                       M20.2196
      CALL RUNTYM(D1,D2)                                                M20.2197
      FLSTAT(3) = FLSTAT(3) + D1                                        M20.2198
      FLSTAT(4) = FLSTAT(4) + D2                                        M20.2199
      FLSTAT(7) = FLSTAT(7) - D1                                        M20.2200
      FLSTAT(8) = FLSTAT(8) - D2                                        M20.2201
      TMAX = FMINFS                                                     M21.  61
      GO TO 9600                                                        M32.3162
 3234 IF ( DELFFS  .LT.  0.0 )   GO TO 3218                             M20.2208
      FMINFS = FMINFS  +  DELFFS                                        M20.2209
      GO TO 3224                                                        M20.2210
 3218 FMINFS = FMINFS * (-DELFFS)                                       M20.2211
 3224 DO 3225  J=1, KCONST                                              M20.2212
      IF ( IFORM(J)  .EQ.  18 )   GO TO 3225                            M32.3163
      SFREQ(J) = FMINFS                                                 M32.3164
 3225 CONTINUE                                                          M32.3165
      IF ( IPRSUP  .GE.  2 )                                            M20.2215
     1 WRITE (LUNIT6, 3227)  KNT, LTLABL, FMINFS, FMAXFS                M36. 710
 3227 FORMAT ( /,  33H BEGIN NEXT FREQ.     KNT  LTLABL,                M36. 711
     1 9X,  6HFMINFS,  9X,  6HFMAXFS  ,/,  17X, 2I8,  2E15.6  )         M36. 712
 1413 IF ( IPRSUP  .GE.  1 )                                            M32.3166
     1 WRITE (LUNIT6, 1417)  KCONST, IBR, INONL, KSWTCH, ISTEAD,        M17. 473
     2                       XOPT, COPT, TWOPI                          M31.1183
 1417 FORMAT ( 1X, 40H  KCONST     IBR   INONL  KSWTCH  ISTEAD,         M22.3229
     1        11X,  4HXOPT,  11X,  4HCOPT,  10X,  5HTWOPI               M31.1184
     2     ,/,  1X,  5I8,  3E15.6  )                                    M31.1185
      IF ( IPRSUP .GE. 1 )                                              VAX.  61
     1 WRITE (LUNIT6, 1419)  ( XOPTBR(J), COPTBR(J), J=1, IBR )         VAX.  62
 1419 FORMAT ( ' X/COPTBR =',  10F12.2 )                                VAX.  63
C     BEFORE STARTING STEADY-STATE SOLUTION FOR INITIAL CONDITIONS, WE  M42.4895
C     MUST FIND PI-EQUIVALENTS FOR DISTRIBUTED BRANCHES.                M42.4896
 1425 ITADD = IT + 1                                                    M21.  63
C     NEXT MAKE SURE OF FREQUENCY SEPARATION WITHIN SUBNETWORKS         M31.1190
C     FOR PHASOR SOLUTION BY CALL TO "FRQCHK" :                         M31.1191
      CALL FRQCHK                                                       M31.1192
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M31.1193
      IPRINT=3                                                          M42.4897
  400 K=1                                                                       
C     ENTER TOP OF LOOP OVER BRANCHES (INDEX "K") FOR PI-EQUIV:         M31.1195
  401 N1=LENGTH(K)                                                              
      IT2=IABS(N1)                                                              
      N15 = IABS ( KBUS(K) )                                                    
      IF ( N15  .EQ.  1 )   N15 = IABS ( MBUS(K) )                      M31.1197
      XOPT = XOPTBR(K)                                                  VAX.  64
      COPT = COPTBR(K)                                                  VAX.  65
      OMLTEM = 1000. / TWOPI                                            VAX.  67
      OMCTEM = 1000. * OMLTEM                                           VAX.  68
      IF( XOPT .GT. 0.0 ) OMLTEM = XOPT * TENM3 * TWOPI *OMLTEM         VAX.  69
      IF( COPT .GT. 0.0 ) OMCTEM = COPT * TWOPI * OMCTEM                VAX.  70
      N16 = IABS ( KSSFRQ(N15) )                                        M31.1198
      D2 = OMLTEM / SFREQ(N16)                                          M31.1199
      H3 = OMCTEM / SFREQ(N16)                                          M31.1200
      OMEGA = TWOPI * SFREQ(N16)                                        M31.1201
      IF(KBUS(K).GT.0) GO TO 411                                               
      IM1=IMODEL(K)  
      IF ( NSOLVE .LT. 1 )  GO TO 40002
      IF ( K .NE. 1 )  GO TO 40002
      IF ( KEXACT .EQ. 88333 .AND. FMINFS .EQ. FMINSV
     1     .AND.  NSOLVE .EQ. 1 )  REWIND LUNIT9
      DO  1234   I=1, IT2
      READ (LUNIT9) DUM, YCHARM, YCHARA, ALPHA, BETA
      IF ( IPRSUP .GE. 1 )
     1 WRITE(LUNIT6,*) 'F,YCHARM,YCHARA,ALPHA,BETA=',
     2                DUM,YCHARM,YCHARA,ALPHA,BETA
      YPOS = EXPZ(ALPHA)
      YNEG = 1.0 / YPOS
      COSHA = 0.5 * ( YPOS + YNEG)
      SINHA = COSHA - YNEG
      SINHGR = SINHA * COSZ(BETA)
      SINHGI = COSHA * SINZ(BETA)
      D9 = 1.0 / YCHARM
      VOLTI(I) = D9 * ( COSZ(YCHARA)*SINHGR + SINZ(YCHARA)*SINHGI ) 
      VOLTK(I) = D9 * ( -SINZ(YCHARA)*SINHGR + COSZ(YCHARA)*SINHGI )
      CC1 = COSHA * COSZ(BETA) - 1.
      CC2 = SINHA * SINZ(BETA)
      D7 = VOLTI(I) ** 2 + VOLTK(I) ** 2
      VOLT(I) = ( VOLTI(I)*CC1 + VOLTK(I)*CC2 ) / D7
      VIM(I+LSIZ26) =  ( VOLTI(I)*CC2 - VOLTK(I)*CC1 ) / D7
      IF (IPRSUP .GE. 1 )
     1 WRITE(LUNIT6,*) ' For mode ', I,' Pi-equiv. Zseres, 1/2 Yshunt=',
     2  VOLTI(I), VOLTK(I), VOLT(I), VIM(I+LSIZ26)
 1234 CONTINUE
      N9SQ = IT2 * IT2
      READ (LUNIT9) (QFD(I),I=1,N9SQ), (SFD(I),I=1,N9SQ)
      IF ( IPRSUP .GE. 1 )
     1 WRITE(LUNIT6,*) ' QFD AND SFD READ FROM LUNIT9.', 
     2     (QFD(I),I=1,N9SQ), (SFD(I),I=1,N9SQ)
      N2 = 1
      GO TO 460
40002 LENGTH(K)=IT2                                                             
      IF ( IPRSUP .GE. 1 )                                              M43. 863
     1 write (lunit6,*) ' over8.  k, imodel(k), it2, omega, h3, d2 =',  M43. 864
     2                            k, imodel(k), it2, omega, h3, d2      M43. 865
C      IF ( KEXACT .EQ. 88333  .AND. NSOLVE .EQ. 0)  KBRNUM = K
      IF ( KODSEM(K) .EQ. 0  .OR.                                       M43. 866
     1     IM1 .EQ. -2 )  GO TO 5136                                            
      IF ( IM1 .EQ. -4 )  GO TO 1536  
      IT2 = IABS(KODEBR(K))                                             M23.1292
C  Formation of phase symmetric PI matrices for components reperesented M23.1293
C  by recursive convolution.  For lumped elements the steady-state modalM23.1294
C  values of the symmetric PI are stored in SCONST(J+0 ... J+3).  for   M23.1295
C  transmiddion lines, SCONST(J+0 ... J+3) holds the modal series Z and M23.1296
C  shunt Y.  (not per unit length)                                      M23.1297
C                                                                       M23.1298
C     (ZPI) = (S)*(DIAG(...SQRT(Z/Y)*SINH(SQRT(Z*Y))...))*(Q)**(-1)     M23.1299
C                                                                       M23.1300
C     (YPI) = (Q)*(DIAG(...SQRT(Y/Z)*TANH(SQRT(Z*Y)/2)...))*(S)**(-1)   M23.1301
C                                                                       M23.1302
C  S(Q) is the mode to phase voltage(current) transformation matrix.    M23.1303
C  Note.  Transpose(S) = Q**(-1)                                        M23.1304
      IF (IPRSUP .LT. 6) GO TO 8009                                     M23.1305
      N1 = ABSZ(CIK(K)) - 1.0                                           M23.1306
      N5 = IT2 * IT2                                                    M23.1307
      N3 = N1 + N5                                                      M23.1308
      N4 = N3 + N5                                                      M23.1309
      WRITE (LUNIT6, 8001) CIK(K)                                       M23.1310
 8001 FORMAT(//,5X,4HSFD(,F4.0,5H) ...,/)                               M23.1311
      DO 8004 I=1, IT2                                                  M23.1312
      N2 = I + N1                                                       M23.1313
      WRITE (LUNIT6, 8002) I, (SFD(J), J=N2, N3, IT2)                   M23.1314
 8002 FORMAT(/,2X,I5,8(1X,E15.7)/(7X,8(1X,E15.7)))                      M23.1315
      N2 = I + N3                                                       M23.1316
      WRITE (LUNIT6, 8003) (SFD(J), J=N2, N4, IT2)                      M23.1317
 8003 FORMAT(7X,8(1X,E15.7))                                            M23.1318
 8004 CONTINUE                                                          M23.1319
      WRITE (LUNIT6, 8005) CIK(K)                                       M23.1320
 8005 FORMAT(///,5X,4HQFD(,F4.0,5H) ...,/)                              M23.1321
      DO 8006 I=1, IT2                                                  M23.1322
      N2 = I + N1                                                       M23.1323
      WRITE (LUNIT6, 8002) I, (QFD(J), J=N2, N3, IT2)                   M23.1324
      N2 = I + N3                                                       M23.1325
      WRITE (LUNIT6, 8003) (QFD(J), J=N2, N4, IT2)                      M23.1326
 8006 CONTINUE                                                          M23.1327
 8009 DO 8090 I=1, IT2                                                          
      II = I + K - 1                                                            
      LENGTH(II) = IT2                                                  M23.1330
      N1 = INDHST(II)                                                           
      IF (CI(II) .GT. 0.0) N1 = N1 + 1                                          
      IF (CNVHST(N1 + 4) .GT. 0.0) GO TO 8700                                   
      N2 = IABS(KODSEM(II)) + 1                                         M23.1334
      N3 = CI(II)                                                       M23.1335
      D1 = 0.0                                                          M23.1336
      D2 = 0.0                                                          M23.1337
      IF (N3) 8300, 8400, 8200                                          M23.1338
 8200 IF (IPRSUP .GE. 9)                                                M23.1339
     1     WRITE(LUNIT6, 8201) N1, N2, N3, SCONST(N2),                  M23.1340
     2     SCONST(N2+1), SCONST(N2+2), D1, D2                           M23.1341
 8201 FORMAT(43H N1, N2, N3, SCONST(N2 ... N2+2), D1, D2 = ,            M28.4270
     1     3I10/5(1X,E15.8))                                            M23.1343
      IF (N3 .LE. 0) GO TO 8250                                         M23.1344
      IF (SCONST(N2)) 8210, 8220, 8230                                  M23.1345
 8210 D5 = SCONST(N2 - 3)                                               M23.1346
      D6 = - SCONST(N2 + 1)                                             M23.1347
      D7 = SCONST(N2 - 2)                                               M23.1348
      D8 = - SCONST(N2 + 2)                                             M23.1349
      GO TO 8240                                                        M23.1350
 8220 D5 = SCONST(N2 + 1)                                               M23.1351
      D6 = 0.0                                                          M23.1352
      D7 = SCONST(N2 + 2)                                               M23.1353
      D8 = 0.0                                                          M23.1354
      GO TO 8240                                                        M23.1355
 8230 D5 = SCONST(N2 + 1)                                               M23.1356
      D6 = SCONST(N2 - 3)                                               M23.1357
      D7 = SCONST(N2 + 2)                                               M23.1358
      D8 = SCONST(N2 - 2)                                               M23.1359
 8240 N3 = N3 - 1                                                       M23.1360
      N2 = N2 + 5                                                       M23.1361
      D9 = D5 * D7 - D6 * D8                                            M23.1362
      D8 = D5 * D8 + D6 * D7                                            M23.1363
      D7 = D9                                                           M23.1364
      D6 = D6 + OMEGA                                                   M23.1365
      D9 = EXPZ( - D5 * CNVHST(N1 - 1))                                 M23.1366
      D10 = - D9 * SINZ(D6 * CNVHST(N1 - 1))                            M23.1367
      D9 = D9 * COSZ(D6 * CNVHST(N1 - 1))                               M23.1368
      D13 = D5 * D5 + D6 * D6                                           M23.1369
      D11 = (D9 * D5 + D10 * D6) / D13                                  M23.1370
      D12 = (- D9 * D6 + D10 * D5) / D13                                M23.1371
      D1 = D1 + D7 * D11 - D8 * D12                                     M23.1372
      D2 = D2 + D7 * D12 + D8 * D11                                     M23.1373
      GO TO 8200                                                        M23.1374
 8250 D4 = D1 * D1 + D2 * D2                                            M23.1375
      D3 = D1 / D4                                                      M23.1376
      D4 = - D2 / D4                                                    M23.1377
      GO TO 8400                                                        M23.1378
 8300 D5 = 0.0                                                          M23.1379
 8310 IF (IPRSUP .GE. 9)                                                M23.1380
     1     WRITE (LUNIT6, 8311) N2, N3, SCONST(N2),                     M23.1381
     2     SCONST(N2+1), D5, D6, D1, D2                                 M23.1382
 8311 FORMAT(34H  N2, N3, SCONST(N2 ... N2+1), D5, ,                    M28.4271
     1     13HD6, D1, D2 = ,2I10/6(1X,E15.8))                           M28.4272
      IF (N3 .GE. 0) GO TO 8320                                         M23.1385
      N3 = N3 + 1                                                       M23.1386
      D6 = - OMEGA * SCONST(N2)                                         M23.1387
      D5 = SCONST(N2 + 1) - D5                                          M23.1388
      D1 = D1 + D5 * COSZ(D6)                                           M23.1389
      D2 = D2 + D5 * SINZ(D6)                                           M23.1390
      D5 = SCONST(N2 + 1)                                               M23.1391
      N2 = N2 + 2                                                       M23.1392
      GO TO 8310                                                        M23.1393
 8320 D6 = - OMEGA * TMAX                                               M23.1394
      D1 = (D1 - D5 * COSZ(D6)) / OMEGA                                 M23.1395
      D2 = - (D2 - D5 * SINZ(D6)) / OMEGA                               M23.1396
      D4 = D1 * D1 + D2 * D2                                            M23.1397
      D3 = D2 / D4                                                      M23.1398
      D4 = - D1 / D4                                                    M23.1399
 8400 N3 = CK(II)                                                       M23.1400
      D1 = 0.0                                                          M23.1401
      D2 = 0.0                                                          M23.1402
      IF (N3) 8500, 8700, 8420                                          M23.1403
 8420 IF (IPRSUP .GE. 9)                                                M23.1404
     1     WRITE (LUNIT6, 8201) N1, N2, N3, SCONST(N2),                 M23.1405
     2     SCONST(N2+1), SCONST(N2+2), D1, D2                           M23.1406
      IF (N3 .LE. 0) GO TO 8470                                         M23.1407
      IF (SCONST(N2)) 8430, 8440, 8450                                  M23.1408
 8430 D5 = SCONST(N2 - 2)                                               M23.1409
      D6 = - SCONST(N2 + 1)                                             M23.1410
      D7 = SCONST(N2 - 1)                                               M23.1411
      D8 = - SCONST(N2 + 2)                                             M23.1412
      GO TO 8460                                                        M23.1413
 8440 D5 = SCONST(N2 + 1)                                               M23.1414
      D6 = 0.0                                                          M23.1415
      D7 = SCONST(N2 + 2)                                               M23.1416
      D8 = 0.0                                                          M23.1417
      GO TO 8460                                                        M23.1418
 8450 D5 = SCONST(N2 + 1)                                               M23.1419
      D6 = SCONST(N2 + 5)                                               M23.1420
      D7 = SCONST(N2 + 2)                                               M23.1421
      D8 = SCONST(N2 + 6)                                               M23.1422
 8460 N3 = N3 - 1                                                       M23.1423
      N2 = N2 + 4                                                       M23.1424
      D11 = D5 * D7 - D6 * D8                                           M23.1425
      D12 = D5 * D8 + D6 * D7                                           M23.1426
      D6 = D6 + OMEGA                                                   M23.1427
      D9 = D5 * D5 + D6 * D6                                            M23.1428
      D1 = D1 + (D11 * D5 + D12 * D6) / D9                              M23.1429
      D2 = D2 + (- D11 * D6 + D12 * D5) / D9                            M23.1430
      GO TO 8420                                                        M23.1431
 8470 N3 = IABS(KODSEM(II))                                             M23.1432
      D1 = D1 + SCONST(N3)                                              M23.1433
      D10 = D1 * D1 + D2 * D2                                           M23.1434
      D9 = D1 / D10                                                     M23.1435
      D10 = - D2 / D10                                                  M23.1436
      GO TO 8600                                                        M23.1437
 8500 D5 = 0.0                                                          M23.1438
      IF (IPRSUP .GE. 9)                                                M23.1439
     1     WRITE (LUNIT6, 8311) N2, N3, SCONST(N2),                     M23.1440
     2     SCONST(N2+1), D5, D6, D1, D2                                 M23.1441
      IF (N3 .GE. 0) GO TO 8320                                         M23.1442
      N3 = N3 + 1                                                       M23.1443
      D6 = - OMEGA * SCONST(N2)                                         M23.1444
      D5 = SCONST(N2 + 1) - D5                                          M23.1445
      D1 = D1 + D5 * COSZ(D6)                                           M23.1446
      D2 = D2 + D5 * SINZ(D6)                                           M23.1447
      D5 = SCONST(N2 + 1)                                               M23.1448
      N2 = N2 + 2                                                       M23.1449
      GO TO 8520                                                        M23.1450
 8520 D5 = -OMEGA * TMAX                                                M23.1451
      N3 = IABS(KODSEM(II))                                             M23.1452
      D1 = (D1 - D5 * COSZ(D6)) / OMEGA                                 M23.1453
      D2 = SCONST(N3) - (D2 - D5 * SINZ(D6)) / OMEGA                    M23.1454
      D10 = D1 * D1 + D2 * D2                                           M23.1455
      D9 = D2 / D10                                                     M23.1456
      D10 = - D1 / D10                                                  M23.1457
 8600 CNVHST(N1 + 0) = D9 * D9 - D10 * D10                              M23.1458
      CNVHST(N1 + 1) = 2.0 * D9 * D10                                   M23.1459
      CNVHST(N1 + 2) = UNITY                                            M23.1460
      CNVHST(N1 + 3) = 0.0                                              M23.1461
      CNVHST(N1 + 4) = OMEGA                                            M23.1462
      GO TO 8070                                                        M23.1463
 8700 CONTINUE                                                          M23.1464
      IF (ABSZ(CNVHST(N1 + 4) - OMEGA) .LT. 0.0001) GO TO 8020          M23.1465
      N2 = -KBUS(II)                                                    M23.1466
      N3 = IABS(MBUS(II))                                               M23.1467
      WRITE (LUNIT6, 8010) BUS(N2), BUS(N3), I, CNVHST(N1 + 4), OMEGA   M23.1468
 8010 FORMAT(//97H WARNING...  STEADY STATE MODAL PARAMETERS FOR RECURSIM23.1469
     1VE-CONVOLUTION COMPONENT CONNECTING NODES ',A6,7H' AND ',A6,1H',/,M23.1470
     2 13X,  9HFOR MODE ,I2, 40H ARE DETERMINED AT ANGULAR FREQUENCY OF M23.1471
     4  ,E12.5,51H RADIANS/SEC.  THE STEADY-STATE SOLUTION FREQUENCY ,/,M23.1472
     5 13X,  14HBEING USED IS ,E12.5,14H RADIANS/SEC.   )               M23.1473
 8020 IF (KODSEM(K) .GT. 0) GO TO 8030                                  M23.1474
      VOLT(I) = CNVHST(N1 + 0)                                          M23.1475
      VOLTI(I) = CNVHST(N1 + 1) * D2                                    M23.1476
      VOLTK(I) = CNVHST(N1 + 2)                                         M23.1477
      NDX1 = LSIZ26 + I                                                 M31.1205
      VIM(NDX1) = CNVHST(N1 + 3) * H3                                   M31.1206
      GO TO 8080                                                        M23.1479
 8030 D3 = CNVHST(N1+0) * CNVHST(N1+2) - CNVHST(N1+1) * CNVHST(N1+3)    M23.1480
      D4 = CNVHST(N1+0) * CNVHST(N1+3) + CNVHST(N1+1) * CNVHST(N1+2)    M23.1481
      D5 = SQRTZ( D3 * D3  +  D4 * D4 )                                 M23.1482
      IF (D3 .LT. 0.0) GO TO 8040                                       M23.1483
      D5 = SQRTZ((D3 + D5) * ONEHAF)                                    M23.1484
      D6 = ONEHAF * D4 / D5                                             M23.1485
      GO TO 8050                                                        M23.1486
 8040 D6 = SQRTZ(( -D3 + D5) * ONEHAF)                                  M23.1487
      IF ( D4 .GE. 0.0) GO TO 18050                                     M29.1532
      DTEMP = -D6                                                       M29.1533
      D5 = ONEHAF * D4 / DTEMP                                          M29.1534
      GO TO 8050                                                        M29.1535
18050 D5 = ONEHAF * D4 / D6                                             M29.1536
 8050 D4 = EXPZ( D5 )                                                   M23.1490
      D3 = D4 * COSZ ( D6 )                                             M23.1491
      D4 = D4 * SINZ ( D6 )                                             M23.1492
      D8 = CNVHST(N1+2) ** 2  +  CNVHST(N1+3) ** 2                      M23.1493
      D7 = (CNVHST(N1+0)*CNVHST(N1+2) + CNVHST(N1+1)*CNVHST(N1+3)) / D8 M23.1494
      D8 = (-CNVHST(N1+0)*CNVHST(N1+3) + CNVHST(N1+1)*CNVHST(N1+2)) / D8M23.1495
      D9 = SQRTZ(D7 * D7  +  D8 * D8)                                   M23.1496
      IF (D7 .LT. 0.0) GO TO 8060                                       M23.1497
      D9 = SQRTZ((D7 + D9) * ONEHAF)                                    M23.1498
      D10 = ONEHAF * D8 / D9                                            M23.1499
      GO TO 8070                                                        M23.1500
 8060 D10 = SQRTZ((-D7 + D9) * ONEHAF)                                  M23.1501
      IF (D8 .LT. 0.0) D10 = - D10                                      M23.1502
      D9 = ONEHAF * D8 / D10                                            M23.1503
 8070 D11 = D3 * D3  +  D4 * D4                                         M23.1504
      D5 = ONEHAF * (D3 - D3 / D11)                                     M23.1505
      D6 = ONEHAF * (D4 + D4 / D11)                                     M23.1506
      VOLT(I) = D9 * D5  - D10 * D6                                     M23.1507
      VOLTI(I) = D9 * D6 + D10 * D5                                     M23.1508
      D3 = ONEHAF * (D3 + D3 / D11) - UNITY                             M23.1509
      D4 = ONEHAF * (D4 - D4 / D11)                                     M23.1510
      D11 = VOLT(I) ** 2  + VOLTI(I) ** 2                               M23.1511
      VOLTK(I) = (VOLT(I) * D3  +  VOLTI(I) * D4) / D11                 M23.1512
      NDX1 = LSIZ26 + I                                                 M31.1207
      VIM(NDX1) = (VOLT(I) * D4  -  VOLTI(I) * D3) / D11                M31.1208
 8080 IF (IPRSUP .LE. 6) GO TO 8090                                     M23.1514
      N2 = - KBUS(II)                                                   M23.1515
      N3 = IABS(MBUS(II))                                               M23.1516
      WRITE (LUNIT6, 8085) BUS(N2), BUS(N3), II, N1, CNVHST(N1 + 0),    M23.1517
     1          CNVHST(N1 + 1), CNVHST(N1 + 2), CNVHST(N1 + 3),         M23.1518
     2          CNVHST(N1 + 4), D3, D4, D5, D6, D7, D8, D9, D10,        M23.1519
     3          VOLT(I), VOLTI(I), VOLTK(I), VIM(NDX1)                  M31.1209
 8085 FORMAT(/,5X,A6,4H TO ,A6,5X,12HBR. INDEX = ,I3,5X,5HN1 = ,I3,/,   M23.1521
     1  1X,29HCNVHST(N1) ... CNVHST(N1+4) =  ,5(1X,E15.7),/,            M23.1522
     2  1X,25HD3, D4, D5, D6, D7, D8 =  ,6(1X,E15.7),/,                 M23.1523
     3  1X,34HD9, D10, VOLT, VOLTI, VOLTK, VIM =   ,6(1X,E15.7)  )      M23.1524
 8090 CONTINUE                                                          M23.1525
      N1 = ABSZ(CIK(K)) - 1.0                                           M23.1526
      N2 = N1 + IT2 * IT2                                               M23.1527
      N6 = 4 * IT2                                                      M23.1528
      DO 8130 I=1, IT2                                                  M23.1529
      N3 = N1 + I                                                       M23.1530
      N4 = N2 + I                                                       M23.1531
      N5 = 1                                                            M23.1532
      NDX1 = LSIZ26                                                     M31.1210
      DO 8100 J=1, IT2                                                  M23.1533
      SEMAUX(N5 + 0) = VOLT(J) * SFD(N3) - VOLTI(J) * SFD(N4)           M23.1534
      SEMAUX(N5 + 1) = VOLT(J) * SFD(N4) + VOLTI(J) * SFD(N3)           M23.1535
      NDX1 = NDX1 + 1                                                   M31.1211
      SEMAUX(N5 + 2) = VOLTK(J) * QFD(N3) - VIM(NDX1) * QFD(N4)         M31.1212
      SEMAUX(N5 + 3) = VOLTK(J) * QFD(N4) + VIM(NDX1) * QFD(N3)         M31.1213
      N3 = N3 + IT2                                                     M23.1538
      N4 = N4 + IT2                                                     M23.1539
      N5 = N5 + 4                                                       M23.1540
 8100 CONTINUE                                                          M23.1541
      II = I + K - 1                                                    M23.1542
      NR(II) = ITADD                                                    M23.1543
      DO 8120 J=1, I                                                    M23.1544
      D3 = 0.0                                                          M23.1545
      D4 = 0.0                                                          M23.1546
      D5 = 0.0                                                          M23.1547
      D6 = 0.0                                                          M23.1548
      N3 = N1 + J                                                       M23.1549
      N4 = N2 + J                                                       M23.1550
      DO 8110 L=1, N6, 4                                                M23.1551
      D3 = D3 + SEMAUX(L + 0) * SFD(N3) - SEMAUX(L + 1) * SFD(N4)       M23.1552
      D4 = D4 + SEMAUX(L + 0) * SFD(N4) + SEMAUX(L + 1) * SFD(N3)       M23.1553
      D5 = D5 + SEMAUX(L + 2) * QFD(N3) - SEMAUX(L + 3) * QFD(N4)       M23.1554
      D6 = D6 + SEMAUX(L + 2) * QFD(N4) + SEMAUX(L + 3) * QFD(N3)       M23.1555
      N3 = N3 + IT2                                                     M23.1556
      N4 = N4 + IT2                                                     M23.1557
 8110 CONTINUE                                                          M23.1558
      IF (ITADD .GT. LDATA) GO TO 9000                                  M23.1559
      TR(ITADD) = D3                                                    M23.1560
      TX(ITADD) = D4 * D2                                               M23.1561
      R(ITADD) = D5                                                     M23.1562
      C(ITADD) = D6 * H3                                                M23.1563
      ITADD = ITADD + 1                                                 M23.1564
 8120 CONTINUE                                                          M23.1565
      IF (IPRSUP .LT. 6) GO TO 8130                                     M23.1566
      N3 = ITADD - 1                                                    M23.1567
      N4 = NR(II)                                                       M23.1568
      WRITE (LUNIT6, 8125) N4, NR(II), II, (TR(J), TX(J), R(J), C(J),   M23.1569
     1          J=N4, N3)                                               M23.1570
 8125 FORMAT(//,5X,5HN4 = ,I4,5X,9HNR(II) = ,I4,5X,5HII = ,I4,5X,       M23.1571
     1  44H(TR(J), TX(J), R(J), C(J), J=N3, N4)  ....  ,/,(8(1X,E15.8)))M23.1572
 8130 CONTINUE                                                          M23.1573
      IT2 = 1                                                           M23.1574
 8140 IF (CKI(K) .LT. 0.0) GO TO 407                                    M23.1575
      K = K + 1                                                         M23.1576
      GO TO 8140                                                        M23.1577
 1536 CONTINUE                                                          M43. 869
      N1 = IT2                                                                  
      N2 = LITYPE(K)
C      IF ( KEXACT .EQ. 88333  .AND. NSOLVE .EQ. 0)  KBRNUM = K
      IF (IM1 .NE. -4 )  GO TO 5137                                             
c      KTRLSW(5) = 1                                                    M43. 873
      IF ( IPRSUP  .GE.  1 )                                            M43. 874
     1 WRITE (LUNIT6, 50002)  OMEGA                                     M43. 875
50002 FORMAT(44H THE FOLLOWING LINES ARE FREQUENCY DEPENDENT,           M43. 876
     1    15H PARAMETERS AT ,  F10.3,  13H RADIANS/SEC.  ,/,            M43. 877
     2   5H BUS1, 7X, 4HBUS2, 14X, 15HSURGE IMPEDANCE, 20X,             M43. 878
     3  21H PROPAGATION FUNCTION, 12X, 11HTRAVEL TIME)                  M43. 879
      DO 11100  I = 1, IT2                                                      
      J = I + K -1                                                              
      NPZ = CKI(J)                                                              
      NPA = CKKJM(J)                                                            
      N3 = KODSEM(J)                                                            
      N8 = INDHST(J)                                                            
      NHST = N8 + 15                                                    M44. 232
C      IF ( CNVHST(NHST) .GT. 0.) GO TO 2015                            M44. 233
      CNVHST(N8+1) = SCONST(N3)                                                 
      CNVHST(N8+2) = 0.0                                                M43. 887
      GJL = 56789.0                                                     M44. 234
      JGL = 0                                                           M44. 235
      IF ( IPRSUP .GE. 1)                                               M44. 236
     1 write(*,1990)                                                    M44. 237
1990  FORMAT(41HNPZ   II   N4    SCONST(N4)    SCONST(N5)               M44. 238
     1      28H  CNVHST(N8+1)  CNVHST(N8+2))                            M44. 239
      DO 30001  II = 1, NPZ                                                     
      N4 = N3 + II                                                      M43. 889
      IF( IMODEL(J) .EQ. -4 ) GO TO 8015                                M44. 240
      if ( ABSZ(sconst(n4))                                                     
     1         .lt. 0.1e17 ) GO TO 8015                                         
      JGL = JGL + 1                                                     M44. 243
      if (jgl .lt. 2 ) gjl = 56789.0                                            
      IF (JGL .Ge. 2 ) gjl = 98765.0                                            
      if (jgl .eq. 2 ) jgl = 0                                                  
      if ( ABSZ(sconst(n4)) .ge. 0.1e15                                         
     1   .and. GJL .EQ. 98765.0 ) go to 30001                                   
      D1 = SCONST(N4)                                                           
      DD1 = SCONST(N4+1)                                                        
      N5 = NPZ + N4                                                             
      DD2 = SCONST(N5)                                                          
      DD3 = SCONST(N5+1)                                                        
      DD3 = DD3 + OMEGA                                                         
      D3 = DD2 ** 2 + DD3 ** 2                                                  
      D1 = D1 * DD2 + DD1 * DD3                                                 
      CNVHST(N8+1)=CNVHST(N8+1)+D1*2/D3                                         
      GO TO 30001                                                               
 8015 D1 = SCONST(N4)                                                           
      N5 = NPZ + N4                                                     M43. 891
      DD2 = SCONST(N5)                                                          
      D3 = DD2 ** 2 + OMEGA ** 2                                                
      CNVHST(N8+1)=CNVHST(N8+1)+D1*DD2/D3                                       
      CNVHST(N8+2) = CNVHST(N8+2)-OMEGA*D1/D3                                   
      IF ( IPRSUP .GE. 1)                                               M44. 259
     1 write(*,1991) NPZ,  II,  N4, SCONST(N4), SCONST(N5),             M44. 260
     2               CNVHST(N8+1), CNVHST(N8+2)                         M44. 261
1991  FORMAT(1X,I2,2X,I3,2X,I6,2X 4E26.9)                                       
30001 CONTINUE                                                          M43. 896
      CNVHST(N8+3) = 0.0                                                M43. 897
      CNVHST(N8+4) = 0.0                                                M43. 898
      JKL = 0                                                           M44. 263
      IF ( IPRSUP .GE. 1)                                               M44. 264
     1 write(*,1988)                                                    M44. 265
1988  FORMAT(41HNPA   II   N6    SCONST(N6)    SCONST(N7)               M44. 266
     1      28H  CNVHST(N8+3)  CNVHST(N8+4))                            M44. 267
      DO 40001 II = 1, NPA                                                      
      N6 = N5 + II                                                      M43. 900
      N7 = N6 + NPA                                                     M43. 902
      IF ( ABSZ( SCONST(N6) ) .GE. 1.E+13 ) GO TO 1989                  M44. 268
      D1 = SCONST(N6)                                                           
      DD2 = SCONST(N7)                                                          
      D3 = DD2 ** 2 + OMEGA ** 2                                        M43. 904
      CNVHST(N8+3) = CNVHST(N8+3) + D1*DD2/D3                                   
      CNVHST(N8+4) = CNVHST(N8+4)-OMEGA*D1/D3                                   
      go to 1993                                                        M44. 269
 1989 JKL = JKL + 1                                                     M44. 270
      IF ( JKL .EQ. 2 ) GO TO 5486                                      M44. 271
      AKR = SCONST(N6) / 1.E+15                                         M44. 272
      AKI = SCONST(N6+1) / 1.E+15                                       M44. 273
      APR = SCONST(N7)   / 1.E+15                                       M44. 274
      API = SCONST(N7+1) / 1.E+15                                               
      DKR = APR * AKR - API * AKI                                               
      DKI = APR * AKI + API * AKR                                               
      AKR = DKR                                                         M44. 278
      AKI = DKI                                                         M44. 279
      DQQ3 = APR ** 2 + ( OMEGA + API ) ** 2                            M44. 280
      DDD3 = ( AKR * APR + AkI  * (OMEGA + API ) ) / DQQ3               M44. 281
      DDD4 = ( AKI * APR - AKR * ( OMEGA + API ) ) / DQQ3               M44. 282
      DQQ4 = APR ** 2 + ( OMEGA - API ) ** 2                            M44. 283
      CCC3 = ( AKR * APR - AKI * ( OMEGA - API ) ) / DQQ4               M44. 284
      CCC4 = ( AKR * ( OMEGA - API ) + AKI * APR ) / DQQ4               M44. 285
      CNVHST(N8+3) = CNVHST(N8+3) + DDD3 + CCC3                         M44. 286
      CNVHST(N8+4) = CNVHST(N8+4) + DDD4 - CCC4                         M44. 287
C      CNVHST(N8+3) = CNVHST(N8+3) + DDD3                               M44. 288
C      CNVHST(N8+4) = CNVHST(N8+4) + DDD4                               M44. 289
      IF ( JKL .EQ. 1 ) GO TO 1993                                      M44. 290
 5486 JKL = 0                                                           M44. 291
 1993 IF ( IPRSUP .GE. 1)                                               M44. 292
     1 write(*,1992) NPA,  II,  N6, SCONST(N6), SCONST(N6+1),           M44. 293
     2       SCONST(N7),SCONST(N7+1),CNVHST(N8+3), CNVHST(N8+4)         M44. 294
1992  FORMAT(1X,I2,2X,I3,2X,I6,2X, 6E19.9)                                      
40001 CONTINUE                                                          M43. 907
      GO TO 2022                                                        M44. 296
2015  NHST = INDHST(K)                                                  M44. 298
      N1 = NHST + 15                                                    M44. 299
      D3 = CNVHST(N1+0) * CNVHST(N1+2)                                          
     1   - CNVHST(N1+1) * CNVHST(N1+3)                                  M44. 301
      D4 = CNVHST(N1+0) * CNVHST(N1+3)                                  M44. 302
     1   + CNVHST(N1+1) * CNVHST(N1+2)                                  M44. 303
      D5 = SQRTZ( D3 * D3  +  D4 * D4 )                                 M44. 304
      IF (D3 .LT. 0.0) GO TO 2017                                       M44. 305
      D5 = SQRTZ((D3 + D5) * ONEHAF)                                    M23.1484
      D6 = ONEHAF * D4 / D5                                             M23.1485
      GO TO 2018                                                        M23.1486
 2017 D6 = SQRTZ(( -D3 + D5) * ONEHAF)                                  M23.1487
      IF ( D4 .GE. 0.0) GO TO 2019                                      M29.1532
      DTEMP = -D6                                                       M29.1533
      D5 = ONEHAF * D4 / DTEMP                                          M29.1534
      GO TO 8050                                                        M29.1535
 2019 D5 = ONEHAF * D4 / D6                                             M29.1536
 2018 D4 = EXPZ( D5 )                                                   M23.1490
      D5 = ONEHAF * D4 / D6                                             M44. 306
      D4 = EXPZ( D5 )                                                   M44. 307
      D3 = D4 * COSZ ( D6 )                                             M44. 308
      D4 = D4 * SINZ ( D6 )                                             M44. 309
      D8 = CNVHST(N1+2) ** 2  +  CNVHST(N1+3) ** 2                      M44. 310
      D7 = (CNVHST(N1+0)*CNVHST(N1+2)                                   M44. 311
     1   + CNVHST(N1+1)*CNVHST(N1+3)) / D8                              M44. 312
      D8 = (-CNVHST(N1+0)*CNVHST(N1+3)                                  M44. 313
     1   + CNVHST(N1+1)*CNVHST(N1+2)) / D8                              M44. 314
      D9 = SQRTZ(D7 * D7  +  D8 * D8)                                   M44. 315
      IF (D7 .LT. 0.0) GO TO 2020                                       M23.1497
      D9 = SQRTZ((D7 + D9) * ONEHAF)                                    M23.1498
      D10 = ONEHAF * D8 / D9                                            M23.1499
      GO TO 2021                                                        M23.1500
 2020 D10 = SQRTZ((-D7 + D9) * ONEHAF)                                  M23.1501
      IF (D8 .LT. 0.0) D10 = - D10                                      M23.1502
      D9 = ONEHAF * D8 / D10                                            M23.1503
 2021 D11 = D3 * D3  +  D4 * D4                                         M23.1504
      D5 = ONEHAF * (D3 - D3 / D11)                                     M44. 316
      D6 = ONEHAF * (D4 + D4 / D11)                                     M44. 317
      VOLT(I) = D9 * D5  - D10 * D6                                     M44. 318
      VOLTI(I) = D9 * D6 + D10 * D5                                     M44. 319
      D3 = ONEHAF * (D3 + D3 / D11) - UNITY                             M44. 320
      D4 = ONEHAF * (D4 - D4 / D11)                                     M44. 321
      D11 = VOLT(I) ** 2  + VOLTI(I) ** 2                               M44. 322
      VOLTK(I) = (VOLT(I) * D3  +  VOLTI(I) * D4) / D11                 M44. 323
      NDX1 = LSIZ26 + I                                                 M44. 324
      VIM(NDX1) = (VOLT(I) * D4  -  VOLTI(I) * D3) / D11                M44. 325
      IF (IPRSUP .LE. 6) GO TO 2022                                     M44. 326
      N2 = - KBUS(II)                                                   M44. 327
      N3 = IABS(MBUS(II))                                               M44. 328
      WRITE (LUNIT6, 2023) BUS(N2), BUS(N3), II, N1, CNVHST(N1 + 0),    M44. 329
     1          CNVHST(N1 + 1), CNVHST(N1 + 2), CNVHST(N1 + 3),         M44. 330
     2          CNVHST(N1 + 4), D3, D4, D5, D6, D7, D8, D9, D10,        M44. 331
     3          VOLT(I), VOLTI(I), VOLTK(I), VIM(NDX1)                  M44. 332
 2023 FORMAT(/,5X,A6,4H TO ,A6,5X,12HBR. INDEX = ,I3,5X,5HN1 = ,I3,/,   M44. 333
     1  1X,29HCNVHST(N1) ... CNVHST(N1+4) =  ,5(1X,E15.7),/,            M44. 334
     2  1X,25HD3, D4, D5, D6, D7, D8 =  ,6(1X,E15.7),/,                 M44. 335
     3  1X,34HD9, D10, VOLT, VOLTI, VOLTK, VIM =   ,6(1X,E15.7)  )      M44. 336
 2022 D4 = CNVHST(N8)                                                           
      D9 = OMEGA * D4                                                           
      D1 = DCOS(D9)                                                             
      DD2 =-DSIN(D9)                                                            
      D3 = CNVHST(N8+3) * D1 - CNVHST(N8+4) * DD2                               
      D5 = CNVHST(N8+4) * D1 + CNVHST(N8+3) * DD2                               
      CNVHST(N8+3) = D3                                                 M43. 914
      CNVHST(N8+4) = D5                                                 M43. 915
C                                                                       M43. 916
      XR1=1.0-D3                                                                
      XI1=-D5                                                                   
      XR2=1.0+D3                                                                
      XI2=D5                                                                    
      D=XR2**2+XI2**2                                                   M43. 921
      BR=(XR1*XR2+XI1*XI2)/D                                                    
      BI=(-XR1*XI2+XI1*XR2)/D                                                   
C                                                                       M43. 924
      D=BR**2+BI**2                                                     M43. 925
      XR1=.5D0*(BR/D-BR)                                                        
      XI1=.5D0*(-BI/D-BI)                                               M43. 927
      N11 = IABS(KBUS(J))                                                       
      N12 = IABS(MBUS(J))                                                       
      J1 = N8 + 1                                                               
      J2 = N8 + 4                                                       M43. 931
      IF ( IPRSUP  .GE.  2 )                                            M43. 932
     1 WRITE (LUNIT6, 70001)  BUS(N11), BUS(N12), (CNVHST(L),           M43. 933
     2                        L=J1, J2),  D4                            M43. 934
70001 FORMAT(1H ,A6,5X,A6,5X,2F15.10,5X,2F15.10,5X,F15.10)              M43. 935
      KOFF1 =900                                                                
      KOFF2 = KOFF1 + ISECTI                                                    
      KOFF3  = KOFF2  + ISECTI                                                  
      KOFF4  = KOFF3  + ISECTI                                                  
      KOFF5  = KOFF4  + ISECTI                                                  
      KOFF6  = KOFF5  + ISECTI                                                  
      KOFF7  = KOFF6  + ISECTI                                                  
      KOFF8  = KOFF7  + ISECTI                                                  
      KOFF9  = KOFF8  + ISECTI                                                  
      KOFF10 = KOFF9  + ISECTI                                                  
C                                                                       M43. 947
C           (1/b-b)*yc/2                                                M43. 948
C                                                                       M43. 949
      WK1(KOFF7+I)=XR1*CNVHST(N8+1)                                     M43. 950
     1              -XI1*CNVHST(N8+2)                                           
      WK1(KOFF9+I)=XR1*CNVHST(N8+2)                                     M43. 952
     1               +XI1*CNVHST(N8+1)                                          
C                                                                       M43. 954
C           b*yc                                                        M43. 955
C                                                                       M43. 956
      WK1(KOFF8+I)=BR*CNVHST(N8+1)-BI*CNVHST(N8+2)                              
      NDX1 = LSIZ26 + I                                                 M43. 958
      WK1(KOFF10+I)=BR*CNVHST(N8+2)                                     M43. 959
     1                        +BI*CNVHST(N8+1)                                  
      IF ( IPRSUP .GE. 1)                                               M43. 961
     1 WRITE (LUNIT6, 90001) I, WK1(KOFF7+I),WK1(KOFF9+I),              M43. 962
     2                          WK1(KOFF8+I),WK1(KOFF10+I)              M43. 963
90001 FORMAT (50H MARTI'S PI-EQUIV(R,X,G,B) PRINTOUT IN OVER8: MODE,    M43. 964
     1        I10, 3X, 4E15.6)                                          M43. 965
11100 CONTINUE                                                          M43. 966
CCCC  DO 952 I=1, IT2                                                   M43. 967
CCCC  IMODEL(I) = -4                                                    M43. 968
CCCC 952   CONTINUE                                                     M43. 969
      GO TO 640                                                         M43. 970
C   5136 IF ( KEXACT .EQ. 88333  .AND. NSOLVE .EQ. 0 )  KBRNUM = K
 5136 N1 = IT2                                                                  
      N2 = LITYPE(K)                                                            
      IF (IM1 .NE. -2 )  GO TO 5137                                             
      KTRLSW(5) = 1                                                     M32.3173
      IF ( IPRSUP  .GE.  1 )                                            M31.1217
     1 WRITE (LUNIT6, 20005)  OMEGA                                     M31.1218
20005 FORMAT(44H THE FOLLOWING LINES ARE FREQUENCY DEPENDENT,           M31.1219
     1    15H PARAMETERS AT ,  F10.3,  13H RADIANS/SEC.  ,/,            M31.1220
     2   5H BUS1, 7X, 4HBUS2, 14X, 15HSURGE IMPEDANCE, 20X,             M31.1221
     3  21H PROPAGATION FUNCTION, 12X, 11HTRAVEL TIME)                  M31.1222
      DO 10110  I = 1, IT2                                                      
      J = I + K -1                                                              
      NPZ = CKI(J)                                                              
      NPA = CKKJM(J)                                                            
      N3 = KODSEM(J)                                                            
      N8 = INDHST(J)                                                    M31.1228
      CNVHST(N8+1) = SCONST(N3)                                                 
      CNVHST(N8+2) = 0.0                                                M31.1230
      DO 10003  II = 1, NPZ                                                     
      N4 = N3 + II                                                      M31.1232
      D1 = SCONST(N4)                                                   M31.1233
      N5 = NPZ + N4                                                     M31.1234
      DD2 = SCONST(N5)                                                  M31.1235
      D3 = DD2 ** 2 + OMEGA ** 2                                                
      CNVHST(N8+1)=CNVHST(N8+1) + D1*DD2/D3                                     
      CNVHST(N8+2) = CNVHST(N8+2) -OMEGA * D1/D3                                
10003 CONTINUE                                                          M31.1239
      CNVHST(N8+3) = 0.0                                                M31.1240
      CNVHST(N8+4) = 0.0                                                M31.1241
      DO 10004 II = 1, NPA                                                      
      N6 = N5 + II                                                      M31.1243
      D1 = SCONST(N6)                                                   M31.1244
      N7 = N6 + NPA                                                     M31.1245
      DD2 = SCONST(N7)                                                  M31.1246
      D3 = DD2 ** 2 + OMEGA ** 2                                        M31.1247
      CNVHST(N8+3) = CNVHST(N8+3) + D1*DD2/D3                                   
      CNVHST(N8+4) = CNVHST(N8+4)-OMEGA*D1/D3                                   
10004 CONTINUE                                                          M31.1250
      D4 = CNVHST(N8)                                                           
      D9 = - OMEGA * D4                                                 M31.1252
      D1 = COSZ(D9)                                                             
      DD2 = SINZ(D9)                                                            
      D3 = CNVHST(N8+3) * D1 - CNVHST(N8+4) * DD2                               
      D5 = CNVHST(N8+3) * DD2 + CNVHST(N8+4) * D1                               
      CNVHST(N8+3) = D3                                                 M31.1257
      CNVHST(N8+4) = D5                                                 M31.1258
      N11 = IABS(KBUS(J))                                                       
      N12 = IABS(MBUS(J))                                                       
      J1 = N8 + 1                                                               
      J2 = N8 + 4                                                       M31.1262
      IF ( IPRSUP  .GE.  2 )                                            M31.1263
     1 WRITE (LUNIT6, 10007)  BUS(N11), BUS(N12), (CNVHST(L),           M31.1264
     2                        L=J1, J2),  D4                            M31.1265
10007 FORMAT(1H ,A6,5X,A6,5X,2F15.10,5X,2F15.10,5X,F15.10)              M31.1266
      D9 = CNVHST(N8+3) ** 2 + CNVHST(N8+4) ** 2                                
      CSIHRE = CNVHST(N8+3) * (1/D9 -1.) * ONEHAF                       M31.1279
      CSIHIM = - CNVHST(N8+4) * (1/D9 +1.) * ONEHAF                     M31.1280
      D3 = 1 + D9 + 2. * CNVHST(N8+3)                                   M31.1281
      CTAHRE = ( 1. - D9 ) / D3                                         M31.1282
      CTAHIM = - 2. * CNVHST(N8+4) / D3                                 M31.1283
      VOLTI(I) = CNVHST(N8+1) * CSIHRE - CNVHST(N8+2) * CSIHIM          M31.1284
      VOLTK(I) = CNVHST(N8+1) * CSIHIM + CNVHST(N8+2) * CSIHRE          M31.1285
      D8 = CNVHST(N8+1) ** 2 + CNVHST(N8+2) ** 2                        M31.1286
      VOLT(I) = (CTAHRE * CNVHST(N8+1) + CTAHIM * CNVHST(N8+2))/D8      M31.1287
      NDX1 = LSIZ26 + I                                                 M31.1288
      VIM(NDX1) = (-CTAHRE*CNVHST(N8+2) + CTAHIM*CNVHST(N8+1))/D8       M31.1289
      IF ( IPRSUP .GE. 1)                                               M31.1290
     1 WRITE (LUNIT6, 10009) I, VOLTI(I),VOLTK(I),VOLT(I),VIM(NDX1)     M31.1291
10009 FORMAT (50H MARTI'S PI-EQUIV(R,X,G,B) PRINTOUT IN OVER8: MODE,    M31.1292
     1        I10, 3X, 4E15.6)                                          M31.1293
10110 CONTINUE                                                          M31.1294
      GO TO 460                                                         M31.1295
 5137 CALL EQUIV ( VOLTI(1), VOLTK(1), VOLT(1), VIM(LSIZ26+1),          M31.1296
     1             CI(K), CK(K), CIK(K), OMEGA, N1 )                    M31.1297
  460 N3=ITADD                                                                  
      IF ( IPRSUP .GE. 1 )                                              M36. 713
     1 WRITE (LUNIT6, 1460) IT2, (VOLTI(I), VOLTK(I), VOLT(I),          M35.1084
     2                      VIM(I+LSIZ26), I = 1, IT2)                 
 1460 FORMAT ( 49H B4 DO 464, VOLTI,VOLTK, VOLT, AND VIM FOR I=1 TO, I3,M37.2934
     1   3HARE, /, (1X, 8E15.6) )                                       M35.1087
      DO 464 I=1,IT2                                                            
      GUS1=VOLTI(I)                                                             
      GUS2=VOLTK(I)                                                             
      GUS3=GUS1*GUS1+GUS2*GUS2                                          M29.1545
      VOLTI(I)=GUS1/GUS3                                                        
  464 VOLTK(I)=-GUS2/GUS3                                                       
C *** Z INVERTED AND STORED IN VOLTI & VOLTK                            M29.1548
      IF ( IPRSUP .GE. 1 )                                              M29.1549
     1 WRITE (LUNIT6, 1465) K,LITYPE(K),IT2,ITADD,N3                    M29.1550
 1465 FORMAT (44H AT 1465, K, LITYPE(K), IT2, ITADD, N3, ARE , 10X,     M37.2935
     1    5I10)                                                         M29.1552
      N3TST = N3 + IT2 * ( IT2 + 1 ) / 2 -1                             M32.3174
      IF (N3TST .GT. LDATA)   GO TO 9000                                M32.3175
      DO 461 I=1,IT2                                                            
      DO 462 J=1,I                                                              
      GUS1=0.                                                           M29.1555
      GUS2=0.                                                           M29.1556
      GUS3=0.                                                           M29.1557
      GUS4=0.                                                           M29.1558
      DO 463 IP=1,IT2                                                           
      NN3=(IP-1)*IT2+N2-1                                               M29.1560
      JIP=NN3+J                                                         M29.1561
      IIP=NN3+I                                                         M29.1562
      H1=QFD(IIP)                                                       M31.1298
      H2=QFD(JIP)                                                               
      HH3 = SFD(IIP)                                                    M31.1300
      H4 = SFD(JIP)                                                             
      D1 = VOLTI(IP) * H2 - VOLTK(IP) * H4                                      
      DD2 = VOLTI(IP) * H4 + VOLTK(IP) * H2                             M31.1303
C  evaluates the matrix product Y = Q * D * Qt                          M43. 971
      D8 = GUS1 + H1*D1 - HH3*DD2                                               
      D9 = GUS2 + H1*DD2 + HH3*D1                                       M31.1305
      GUS1 = D8                                                                 
      GUS2 = D9                                                         M31.1307
      NDX1 = LSIZ26 + IP                                                M31.1308
      D3 = VOLT(IP) * H2 - VIM(NDX1) * H4                                       
      D4 = VOLT(IP) * H4 + VIM(NDX1) * H2                               M31.1310
      D8 = GUS3 + H1*D3 - HH3*D4                                                
      D9 = GUS4 + H1*D4 + HH3*D3                                        M31.1312
      GUS3 = D8                                                                 
      GUS4 = D9                                                         M31.1314
  463 CONTINUE                                                                  
C *** CALCULATION OF PI CIRCUIT PARAMETERS (ONLY UPPER PART OF SYMMETRICM29.1569
C     MATRIX IS CALCULATED)                                             M29.1570
      R(ITADD)=GUS3                                                             
      C(ITADD)=GUS4                                                             
      TR(ITADD)=GUS1                                                            
      TX(ITADD)=GUS2                                                            
      IF ( IPRSUP .GE. 1 )                                              M36. 714
     1  WRITE (LUNIT6, 1462) ITADD,  GUS3, GUS4, GUS1, GUS2             M35.1089
 1462 FORMAT ( 46H GUS3, GUS4, GUS1 AND GUS2 AT 1462 FOR ITADD =, I3,   M35.1090
     1 3HARE, 4E15.6 )                                                  M35.1091
  462 ITADD=ITADD                                                       M29.1575
     1 +1                                                                       
      NR(K)=ITADD-I                                                             
  461 K=K+1                                                                     
      N4=ITADD-1                                                        M29.1578
      LL0 = 0                                                           M29.1579
      IF ( IPRSUP .GE. 1 )                                              M36. 715
     1 WRITE (LUNIT6, 3465) N3, ( TR(I), TX(I), I=1, N3)                M35.1092
 3465 FORMAT ( 38H B4 CALL CXRED8, TR AND TX FOR I =1 TO, I4, 4H ARE,   M35.1093
     1        /, (1X, 8E15.6) )                                         M35.1094
      CALL CXRED8(TR(N3),TX(N3),IT2,LL0)                                        
      IF ( IPRSUP .GE. 1 )                                              M36. 716
     1 WRITE ( LUNIT6, 2465 ) N3, (TR(I), TX(I), I=1,N3)                M35.1095
 2465 FORMAT ( 41H AFTER CALL CXRED8, TR AND TX FOR I =1 TO, I4, 4H ARE,M35.1096
     1        /, (1X, 8E15.6) )                                         M35.1097
C *** CONVERT X & C TO CONFORM TO XOPT & COPT OPTIONS                   M29.1581
      DO 465 I=N3,N4                                                    M29.1582
      TR(I)=-TR(I)                                                      M29.1583
      TX(I)=-TX(I)*D2                                                           
  465 C(I)=C(I)*H3                                                              
      IT2=0                                                                     
      GO TO 407                                                         M42.4901
C                                                                       M43. 973
  640 INOFF2 = 2 * LBRNCH                                                       
      INOFF1 = LBRNCH                                                           
      NQ1=INFDLI(INOFF2+K)                                                      
      KOFF13 = KOFF10 + ISECTI                                                  
      KOFF14 = KOFF13 + ISECTI                                                  
      KOFF15 = KOFF14 + ISECTI                                                  
      KOFF16 = KOFF15 + ISECTI                                                  
      KOFF17 = KOFF16 + ISECTI                                                  
      KOFF18 = KOFF17 + ISECTI                                                  
      KOFF19 = KOFF18 + ISECTI                                                  
      KOFF20 = KOFF19                                                           
      KQVV = INFDLI(INOFF1+K)                                           M43. 987
      NPHS2=IT2*IT2                                                     M43. 989
      DO 609 KQV=1,NPHS2                                                        
      NTEQ = WK1(KOFF20+KQVV)                                                   
       XK(KQV)=SCONST(NQ1)                                                      
       XM(KQV)=0.D0                                                     M44. 729
      IF ( IPRSUP .GE. 1)                                               M43. 994
     1 write(*,5692)                                                    M43. 995
5692  FORMAT(43HkQV NTEQ    NQ1    NQ2    NQ3   SCONST(NQ1),            M43. 996
     1      42H   SCONST(NQ2)   SCONST(NQ3)       QR(KQV))              M43. 997
      DO 801 LQ=1,NTEQ                                                          
      NQ2 = NQ1 + LQ                                                            
      NQ3 = NQ2 + NTEQ                                                          
      IF ( ABSZ( SCONST(NQ2) ) .GE. 1.E+13 ) GO TO 3000                 M44. 338
      D = OMEGA ** 2 + SCONST(NQ3) ** 2                                         
       XK(KQV) =  XK(KQV) + SCONST(NQ2) * SCONST(NQ3) / D               M43.1005
       XM(KQV) =  XM(KQV) - SCONST(NQ2) * OMEGA / D                     M43.1006
      GO TO 3001                                                        M44. 339
 3000 JKL = JKL + 1                                                     M44. 340
      IF ( JKL .EQ. 2 ) GO TO 3002                                      M44. 341
      AKR = SCONST(NQ2) / 1.E+15                                        M44. 342
      AKI = SCONST(NQ2+1) / 1.E+15                                      M44. 343
      APR = SCONST(NQ3)   / 1.E+15                                      M44. 344
      API = SCONST(NQ3+1) / 1.E+15                                              
      DQQ3 = APR ** 2 + ( OMEGA + API ) ** 2                            M44. 346
      DDD3 = ( AKR * APR + AkI  * (OMEGA + API ) ) / DQQ3               M44. 347
      DDD4 = ( AKI * APR - AKR * ( OMEGA + API ) ) / DQQ3               M44. 348
      XK(KQV) = XK(KQV) + 2*DDD3                                        M44. 349
      IF ( JKL .EQ. 1 ) GO TO 1993                                      M44. 350
 3002 JKL = 0                                                           M44. 351
 3001 IF ( IPRSUP .GE. 1)                                               M43.1008
     1 write(*,1333) kQV,NTEQ,NQ1,NQ2,NQ3,SCONST(NQ1),                  M43.1009
     1             SCONST(NQ2),SCONST(NQ3), XK(KQV)                     M43.1010
1333  FORMAT ( 1X,I2,3X,I2,2X,I5,2X,I5,2X,I5, 4E14.5)                   M43.1011
      NQ2 = NQ2 + 1                                                     M43.1012
      NQ3 = NQ3 + 1                                                     M43.1013
801   CONTINUE                                                          M43.1014
      NQ1 = NQ1 + 2 * NTEQ + 1                                          M43.1015
      KQVV = KQVV + 1                                                   M44. 352
609   CONTINUE                                                          M43.1016
      IF ( IPRSUP .GE. 1)                                               M43.1017
     1 WRITE (lunit6,*) 'Following are QR(k) by rows____________'       M43.1018
      IF ( IPRSUP .GE. 1)                                               M43.1019
     1 WRITE (lunit6,4515)   (  XK(J), J=1, nphs2 )                     M43.1020
      IF ( IPRSUP .GE. 1)                                               M43.1021
     1 WRITE (lunit6,*) 'Following are QI(k) by rows____________'       M43.1022
      IF ( IPRSUP .GE. 1)                                               M43.1023
     1 WRITE (lunit6,4516)   (  XM(J), J=1, nphs2 )                     M43.1024
 4515 FORMAT ( 7H+ Qr-I:, 3E14.5 )                                      M43.1025
 4516 FORMAT ( 7H+ Qi-I:, 3E14.5 )                                      M43.1026
C                                                                       M43.1027
      DO 933 IK=1, NPHS2                                                M43.1028
      IF ( IPRSUP  .GE.  3 )                                            M43.1029
     1 WRITE (LUNIT6, 4160) NPHS2, XK(IK), XM(IK)                       M43.1030
 4160 FORMAT ( 24HNPHS2 & QR(IK) & QI(IK)=,I3, 2X, 2E15.6 )             M43.1031
 933  CONTINUE                                                          M43.1032
C                                                                       M43.1033
C        Evaluate     Ypi = Q * b*yc * Qt                               M43.1034
C                 Zpi**-1 = Q * (1/b-b)*yc/2 * Qt                       M43.1035
C                                                                       M43.1036
 111  NN1 = IT2 * ( IT2 + 1 ) / 2                                       M43.1037
      NN2 = ITADD - 1                                                   M43.1038
      DO 1008 IQY=1, NN1                                                M43.1039
      R(NN2+IQY) = 0.D0                                                 M43.1040
      C(NN2+IQY) = 0.D0                                                 M43.1041
1008   CONTINUE                                                         M43.1042
      DO 1018 KQY=1,IT2                                                         
      KM1 = ( KQY - 1 ) * IT2                                                   
      DYRK = WK1(KOFF8+KQY)                                             M43.1045
      DYIK = WK1(KOFF10+KQY)                                            M43.1046
      IJ=0                                                              M43.1047
      DO 1027 JQY =1, IT2                                                       
      JK = KM1 + JQY                                                            
      QRJK =  XK(JK)                                                    M43.1050
      QIJK =  XM(JK)                                                    M43.1051
      DO 1037 IQY = 1, JQY                                              M43.1052
      IJ = IJ + 1                                                       M43.1053
      IKQ = KM1 + IQY                                                           
      QRIK =  XK(IKQ)                                                   M43.1055
      QIIK =  XM(IKQ)                                                   M43.1056
      BR = QRIK * QRJK - QIIK * QIJK                                    M43.1057
      BI = QRIK * QIJK + QIIK * QRJK                                    M43.1058
      R(NN2+IJ) = R(NN2+IJ)                                             M43.1059
     1             + DYRK * BR - DYIK * BI                              M43.1060
      C(NN2+IJ) = C(NN2+IJ)                                             M43.1061
     1             + DYRK * BI + DYIK * BR                              M43.1062
1037   CONTINUE                                                         M43.1063
1027   CONTINUE                                                         M43.1064
1018   CONTINUE                                                         M43.1065
      DO 1007 IQY=1, NN1                                                M43.1067
      TR(NN2+IQY) = 0.D0                                                M43.1068
      TX(NN2+IQY) = 0.D0                                                M43.1069
1007   CONTINUE                                                         M43.1070
      DO 1017 KQY=1,IT2                                                         
      KM1 = ( KQY - 1 ) * IT2                                                   
      DYRK = WK1(KOFF7+KQY)                                             M43.1073
      DYIK = WK1(KOFF9+KQY)                                             M43.1074
      IJ=0                                                              M43.1075
      DO 102 JQY =1, IT2                                                        
      JK = KM1 + JQY                                                            
      QRJK =  XK(JK)                                                    M43.1078
      QIJK =  XM(JK)                                                    M43.1079
      DO 103 IQY = 1, JQY                                               M43.1080
      IJ = IJ + 1                                                       M43.1081
      IKQ = KM1 + IQY                                                           
      QRIK =  XK(IKQ)                                                   M43.1083
      QIIK =  XM(IKQ)                                                   M43.1084
      BR = QRIK * QRJK - QIIK * QIJK                                    M43.1085
      BI = QRIK * QIJK + QIIK * QRJK                                    M43.1086
      TR(NN2+IJ) = TR(NN2+IJ)                                           M43.1087
     1             + DYRK * BR - DYIK * BI                              M43.1088
      TX(NN2+IJ) = TX(NN2+IJ)                                           M43.1089
     1             + DYRK * BI + DYIK * BR                              M43.1090
103   CONTINUE                                                          M43.1091
102   CONTINUE                                                          M43.1092
1017  CONTINUE                                                          M43.1093
      IT0 = IT2 * ( IT2 + 1 ) / 2                                               
      IF ( IPRSUP  .GE.  3 )                                            M43.1095
     1 WRITE(LUNIT6,5284)(I,TR(I),TX(I),R(I),C(I),                      M43.1096
     2                         I=ITADD,ITADD+IT0)                       M43.1097
 5284 FORMAT ( 7X,  3HROW,  13X,  2HTR,  13X,  2HTX,  14X,              M43.1098
     1 1HR,  14X,  1HC  ,/,  ( I10,  4E15.5  ) )                        M43.1099
      DO 932 Ik=1 , IT2                                                 M43.1100
      NR(K) = ITADD + ( Ik - 1 ) * Ik / 2                               M43.1101
      K = K + 1                                                         M43.1102
  932 CONTINUE                                                          M43.1103
      ITADD = ITADD + IT0                                               M43.1104
C                                                                       M43.1105
      GO TO 393                                                         M43.1106
 411  if(N1.LT.0) GO TO 407                                                     
      I=NR(K)                                                                   
      IF(I.LT.0) GO TO 407                                                      
      N2=I+IT2*(IT2+1)/2-1                                                      
      DO 412 L=I,N2                                                     M42.4906
  412 C(L) = C(L) * ONEHAF                                              M31.1316
  407 K=K+IT2                                                                   
  393 IF ( K .LE. IBR ) GO TO 401                                               
      N13 = 1                                                           M32.3176
      IF (KTRLSW(5) .EQ. 1) CALL VECRSV (CNVHST(1), LHIST, N13)         M32.3177
      I = IT + 1                                                                
      J0 = ITADD - 1                                                    M42.4909
      IF ( LASTOV .EQ. NCHAIN - 1  .OR.  IPRSUP .GE. 1 )                M38.1651
     1 WRITE (LUNIT6, 47884)  I, J0                                     M38.1652
47884 FORMAT (   68H PI-EQUIV BRANCHES OF DISTRIB LINES IN TR, TX, ETC. M38.1653
     1 BETWEEN LIMITS    , 2I6 )                                        M42.4910
      IF ( IPRSUP  .GE.  3 )                                            M16.1382
     1 WRITE (LUNIT6, 2584)  ( I, TR(I), TX(I), R(I), C(I), I=1, J0 )   M17. 483
 2584 FORMAT ( 7X,  3HROW,  13X,  2HTR,  13X,  2HTX,  14X,              M16.1384
     1 1HR,  14X,  1HC  ,/,  ( I10,  4E15.5  ) )                        M16.1385
      IF(INONL.EQ.0) GO TO 414                                                  
      IF ( NOUTPR  .EQ.  0 )                                            M20.2237
     1 WRITE(LUNIT6,406)                                                M20.2238
  406 FORMAT(121H NONLINEAR AND TIME-VARYING RESISTANCES IGNORED IN STEAM42.4912
     1DY STATE SOLUTION. NONLINEAR INDUCTANCES INCLUDED WITH LINEAR PARTM42.4913
     2)                                                                 M42.4914
      D2=1000.0                                                         M42.4915
      IF( XOPT .GT. 0.0 )  D2 = XOPT * TWOPI                            M42.4916
      IF ( IPRSUP  .LE.  1 )   GO TO 4265                               M14.1330
      WRITE (LUNIT6, 4261)  ( I, NAMENL(I), NLTYPE(I), NONLK(I),        M42.4917
     1        NONLM(I), NONLAD(I), NONLE(I), ILAST(I), VNONL(I),        M33. 618
     2                   CURR(I), ANONL(I), VZERO(I), I=1, INONL )      M14.1333
 4261 FORMAT ( /,  25H NONLINEAR ELEMENT TABLE.  ,/,                    M42.4918
     1   36H ROW  NAMENL  NLTYPE   NONLK   NONLM,                       M42.4919
     1   24H  NONLAD   NONLE   ILAST,                                   M33. 620
     2 10X, 5HVNONL, 11X, 4HCURR, 10X, 5HANONL, 10X, 5HVZERO, /,        M14.1336
     3 ( 1X, I3,  7I8, 4E15.6 ) )                                       M42.4920
      WRITE (LUNIT6, 4262)  ( I, CCHAR(I), VCHAR(I), GSLOPE(I),         M22.3234
     1  I=1, ICHAR )                                                    M22.3235
 4262 FORMAT ( /,  43H NONLINEAR ELEMENT CHARACTERISTICS.     ROW,      M14.1339
     1 10X, 5HCCHAR, 10X, 5HVCHAR, 9X, 6HGSLOPE, /,                     M14.1340
     2 ( 35X, I8, 3E15.6 ) )                                            M14.1341
 4265 IPRINT = 2                                                        M14.1342
      DO 413 I=1,INONL                                                  M42.4921
      IF( NLTYPE(I) .GT. 0 )  GO TO 4269                                M42.4922
      IF ( NLTYPE(I) .EQ. -98   .OR.                                    M23.1578
     1     NLTYPE(I) .EQ. -96 )  GO TO 4269                             M23.1579
      IF( ANONL(I) .GE. 0.0 )  GO TO 413                                M42.4923
      GO TO 4272                                                        M42.4924
 4269 J0 = NONLE(I)                                                     M42.4925
      IF(J0.GT.0) GO TO 413                                             M42.4926
      IF(VZERO(I).EQ.0.) GO TO 413                                      M42.4927
      IF( ANONL(I) .GT. 0.0 )  GO TO 4272                               M42.4928
      KILL = 14                                                         M42.4929
      LSTAT(19) = 406                                                   M42.4930
      FLSTAT(16) = ANONL(I)                                             M42.4931
      N1 = NONLK(I)                                                     M42.4932
      N2 = IABS( NONLM(I) )                                             M42.4933
      BUS1 = BUS(N1)                                                    M42.4934
      BUS2 = BUS(N2)                                                    M42.4935
      GO TO 9200                                                        M42.4936
 4272 CALL IBRINC                                                       VAX.  71
      IF( IPRSUP .GE. 3 )                                               M42.4938
     1 WRITE(LUNIT6, 65457)  ( K, KBUS(K), MBUS(K), NR(K), LENGTH(K),   M17. 488
     2 KODEBR(K), K=1, IBR )                                            M42.4939
65457 FORMAT(  32H BRANCH TABLE INTEGERS AT 65457.   ,/, ( 1X, 6I10 ) ) M42.4940
      LSTAT(19)= 4272                                                   M42.4941
      IF(IBR  .GT.LBRNCH) GO TO 9000                                    M42.4942
      NR(IBR)  =-ITADD                                                  M42.4943
      LENGTH(IBR)  =1                                                   M42.4944
      KBUS(IBR)  =NONLK(I)                                              M42.4945
      MBUS(IBR)  =IABS(NONLM(I))                                        M42.4946
      IF ( NLTYPE(I)  .NE.  -97     .AND.                               M22.3236
     1     NLTYPE(I)  .NE.  -99 )   GO TO 65464                         M22.3237
      L = NONLAD(I)                                                     M33. 622
      TR(ITADD) = 1.0 / GSLOPE(L)                                       M42.4947
      TX(ITADD) = 0.0                                                   M42.4948
      GO TO 65468                                                       M42.4949
65464 TR(ITADD) = 0.0                                                   M42.4950
      TX(ITADD)=ANONL(I)/VZERO(I)*D2                                    M42.4951
65468 C(ITADD) = 0.0                                                    M42.4952
      ITADD=ITADD+1                                                     M42.4953
  413 CONTINUE                                                          M42.4954
  414 LASTOV = NCHAIN                                                           
      NCHAIN = NCHAIN + 1                                                       
      LSTAT(23) = ITADD - 1                                             M42.4955
      LSTAT(22) = IBR                                                   M32.3179
      IF ( FMAXFS .GT. 0.0  .AND.  KNT .GT. 1 )  GO TO 9600             M32.3180
      GO TO 8900                                                        M32.3181
 9500 IF ( KCONST  .LT.  LEXCT )   GO TO 1517                           M32.3182
C     TEMPORARY ERROR STOP --- UNABLE TO ADD ANOTHER SOURCE.            M32.3183
      WRITE (LUNIT6, 1514)                                              M32.3184
 1514 FORMAT (   39H TOO MANY SOURCES.  S.N. 1514 "FRQCHK". )           M32.3185
      CALL STOPTP                                                               
 1517 N13 = KCONST + 1                                                  M32.3187
      SFREQ(N13) = 1.0                                                  M32.3188
      CALL COPYI ( N13, KSSFRQ(1), NTOT )                               M32.3189
      IF( TMAX .GT. -DELTA2 )  GO TO 1534                               M32.3190
      KILL = 74                                                         M42.4956
      LSTAT(19) = 1517                                                  M32.3191
      FLSTAT(15) = TMAX + DELTA2                                        M42.4957
      LSTAT(16) = KCONST                                                M42.4958
      GO TO 9200                                                        M42.4959
 1534 LASTOV = NCHAIN                                                   M32.3192
      NCHAIN = 12                                                       M32.3193
 8900 CALL RUNTYM ( D1, D2 )                                                    
      FLSTAT(1) = FLSTAT(1) + D1                                                
      FLSTAT(2) = FLSTAT(2) + D2                                                
      FLSTAT(3) = FLSTAT(3) - D1                                        M20.2244
      FLSTAT(4) = FLSTAT(4) - D2                                        M20.2245
      GO TO 9600                                                        M32.3195
 9000 LSTAT(16) = IPRINT                                                M42.4960
      KILL = 1                                                          M42.4961
 9200 LASTOV = NCHAIN                                                   M15. 752
      NCHAIN = 51                                                       M15. 753
      LSTAT(18) = 8                                                     M42.4962
 9600 IF ( IPRSUP  .GE.  1 )                                            M32.3196
     1 WRITE (LUNIT6, 1786) NCHAIN, KCONST, KILL, ITADD, FMAXFS         M32.3197
 1786 FORMAT ( 16H EXIT "OVER8".   ,                                    M37.2937
     1  38H NCHAIN, KCONST, KILL, ITADD, FMAXFS =, 4I8, E14.4 )         M32.3199
99999 RETURN                                                            M42.4963
      END                                                               M42.4964
      FUNCTION  INDBLK(TEMPBUS)
      CHARACTER*6  TEMPBUS
      INDBLK = INDEX ( TEMPBUS, ' ' )
      RETURN
      END
      SUBROUTINE FRQCHK                                                 M31.1317
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     MODULE CALLED IN TWO PLACES BY "OVER8" TO DETERMINE WHICH         M31.1318
C     NODES ARE EXCITED BY WHICH SOURCES (SUBNETWORK BOUNDING),         M31.1319
C     WITH ERROR STOP IF THERE IS FREQUENCY MIXING.                     M31.1320
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      IF ( IPRSUP  .GE.  1 )                                            M31.1323
     1 WRITE (LUNIT6, 1428)    NTOT, KCONST, IBR, KSWTCH, IT            M31.1324
 1428 FORMAT (    17H TOP OF "FRQCHK".,                                 M31.1325
     1            40H    NTOT  KCONST     IBR  KSWTCH      IT           M31.1326
     2            ,/,  17X,  8I8  )                                     M31.1327
C     FIRST DETERMINE THE PHASOR FREQUENCY OF EACH NETWORK NODE:        M31.1328
      CALL MOVE0 ( KSSFRQ(1), NTOT )                                    M31.1329
      N13 = 0                                                           M31.1330
      N11 = 0                                                           M37.2938
      N14 = 0                                                           M37.2939
      D12 = 0.0                                                         M31.1331
      N16 = 0                                                           M31.1332
      DO 1438  I=1, KCONST                                              M31.1333
      N4 = IABS ( NODE(I) )                                             M31.1334
      KPSOUR(I) = I                                                     M31.1335
      IF ( KSSFRQ(N4)  .GT.  0 )        GO TO 1438                      M31.1336
      IF ( IABS(IFORM(I))  .NE.  14 )   GO TO 1438                      M31.1337
      IF ( TSTART(I)  .EQ.  5432. )   GO TO 1434                        M31.1338
      IF ( TSTART(I)  .GE.  0.0 )       GO TO 1438                      M31.1339
 1434 KSSFRQ(N4) = I                                                    M31.1340
      IF ( IFORM(I+1)  .NE.  18 )   GO TO 1436                          M32.3200
      N4 = NODE(I+1)                                                    M32.3201
      KSSFRQ(N4) = I                                                    M32.3202
 1436 IF ( N13  .EQ.  0 )   N13 = I                                     M32.3203
      IF ( D12  .EQ.  0.0 )   D12 = SFREQ(I)                            M31.1342
      IF ( SFREQ(I)  .EQ.  D12 )   GO TO 1438                           M31.1343
      N16 = 1                                                           M31.1344
 1438 CONTINUE                                                          M31.1345
      IF ( N16  .EQ.  0 )   GO TO 1480                                  M31.1346
 1441 K = 1                                                             M31.1347
      N8 = 0                                                            M31.1348
 1446 N6 = IABS( LENGTH(K) )                                            M31.1349
      N7 = K                                                            M31.1350
      DO 1456  J=1, N6                                                  M31.1351
      N1 = IABS( KBUS(N7) )                                             M31.1352
      N2 = IABS( MBUS(N7) )                                             M31.1353
      IF ( KSSFRQ(N1)  .EQ.  0 )   GO TO 1453                           M31.1354
      IF ( N1  .EQ.  1 )           GO TO 1453                           M31.1355
      N12 = KSSFRQ(N1)                                                  M31.1356
      GO TO 1458                                                        M31.1357
 1453 IF ( KSSFRQ(N2)  .EQ.  0 )   GO TO 1456                           M31.1358
      IF ( N2  .EQ.  1 )           GO TO 1456                           M31.1359
      N12 = KSSFRQ(N2)                                                  M31.1360
      GO TO 1458                                                        M31.1361
 1456 N7 = N7 + 1                                                       M31.1362
      K = K + N6                                                        M31.1363
      GO TO 1469                                                        M31.1364
 1458 D12 = SFREQ(N12)                                                  M31.1365
      DO 1467  J=1, N6                                                  M31.1366
      N2 = 0                                                            M31.1367
      N1 = IABS( KBUS(K) )                                              M31.1368
 1461 N16 = KSSFRQ(N1)                                                  M31.1369
      IF ( N1  .LE.  1 )   GO TO 2465                                   M31.1370
      IF ( N16  .LE.  0 )   GO TO 1464                                  M31.1371
      IF ( SFREQ(N16)  .EQ.  D12 )   GO TO 1462                         M31.1372
      KILL = 222                                                        M31.1373
      N11 = N11 + 1                                                     M37.2940
      IF (N11 .GT. 1) GO TO 1462                                        M37.2941
      N5 = K                                                            M37.2942
      N10 = N12                                                         M37.2943
      N15 = IABS ( KBUS(K) )                                            M37.2944
      N20 = IABS ( MBUS(K) )                                            M37.2945
      BUS1 = BUS(N1)                                                    M31.1377
      BUS2 = BUS(N2)                                                    M31.1378
 1462 IF ( KPSOUR(N12)  .LT.  KPSOUR(N16) )   GO TO 1463                M31.1380
      KPSOUR(N12) = KPSOUR(N16)                                         M31.1381
      GO TO 2465                                                        M31.1382
 1463 KPSOUR(N16) = KPSOUR(N12)                                         M31.1383
      GO TO 2465                                                        M31.1384
 1464 KSSFRQ(N1) = N12                                                  M31.1385
      N8 = N8 + 1                                                       M31.1386
 2465 IF ( N2  .GT.  0 )   GO TO 1467                                   M31.1387
      N2 = 1                                                            M31.1388
      N1 = IABS( MBUS(K) )                                              M31.1389
      GO TO 1461                                                        M31.1390
 1467 K = K + 1                                                         M31.1391
      IF ( IPRSUP  .GE.  6 )                                            M31.1392
     1 WRITE (LUNIT6, 1468)  K, LENGTH(K), N6, N8, N12, D12             M31.1393
 1468 FORMAT (  33H K, LENGTH(K), N6, N8, N12, D12 =,                   M31.1394
     1  5I6,  F10.2  )                                                  M31.1395
 1469 IF ( K  .LE.  IBR )   GO TO 1446                                  M31.1396
      IF ( KSWTCH  .LE.  0 )   GO TO 1475                               M31.1397
      DO 1474  J=1, KSWTCH                                              M31.1398
      IF ( ADELAY(J)  .EQ.  -44444. )   GO TO 1470                      M31.1399
      IF ( TCLOSE(J)  .GE.  0.0 )     GO TO 1474                        M31.1400
      IF ( ADELAY(J)  .EQ.  44444. )  GO TO 1474                        M31.1401
 1470 N1 = IABS( KMSWIT(J) )                                            M32.3207
      NDX1 = LSWTCH + J                                                 M32.3208
      N2 = IABS( KMSWIT(NDX1) )                                         M32.3209
      IF ( N1  .EQ.  1 )   GO TO 1474                                   M31.1404
      IF ( N2  .EQ.  1 )   GO TO 1474                                   M31.1405
      N16 = KSSFRQ(N1)                                                  M31.1406
      N17 = KSSFRQ(N2)                                                  M31.1407
      IF ( N16+N17  .EQ.  0 )   GO TO 1474                              M31.1408
      IF ( N16*N17  .EQ.  0 )   GO TO 1473                              M31.1409
      IF ( SFREQ(N16)  .EQ.  SFREQ(N17) )   GO TO 1471                  M31.1410
      KILL = 223                                                        M31.1411
      N14 = N14 + 1                                                     M37.2946
      IF (N14 .GT. 1) GO TO 1471                                        M37.2947
      N9 = J                                                            M37.2948
      BUS3 = BUS(N1)                                                    M37.2949
      BUS4 = BUS(N2)                                                    M37.2950
      N18 = KSSFRQ(N1)                                                  M37.2951
      N19 = KSSFRQ(N2)                                                  M37.2952
 1471 IF ( KPSOUR(N16)  .LT.  KPSOUR(N17) )                             M32.3210
     1 KPSOUR(N17) = KPSOUR(N16)                                        M32.3211
      IF ( KPSOUR(N16)  .GT.  KPSOUR(N17) )                             M32.3212
     1 KPSOUR(N16) = KPSOUR(N17)                                        M32.3213
      GO TO 1474                                                        M32.3214
 1473 N8 = N8 + 1                                                       M31.1423
      IF ( KSSFRQ(N1)  .EQ.  0 )   KSSFRQ(N1) = KSSFRQ(N2)              M31.1424
      IF ( KSSFRQ(N2)  .EQ.  0 )   KSSFRQ(N2) = KSSFRQ(N1)              M31.1425
 1474 CONTINUE                                                          M31.1426
 1475 IF ( IPRSUP  .GE.  5 )                                            M31.1427
     1 WRITE (LUNIT6, 1476)  N8,  ( KSSFRQ(J), J=1, NTOT )              M31.1428
 1476 FORMAT ( 39H ADDITIONAL FREQUENCY SPREADING.   N8 =,  I5,         M31.1429
     1         42H      (KSSFRQ(J), J=1, NTOT)  FOLLOWS ....    ,/,     M31.1430
     2   ( 1X, 20I4 ) )                                                 M31.1431
      IF ( IPRSUP  .GE.  5 )                                            M31.1432
     1 WRITE (LUNIT6, 1478)  ( KPSOUR(J), J=1, KCONST )                 M31.1433
 1478 FORMAT (  8H KPSOUR:, 20I5  )                                     M31.1434
      IF ( N8  .GT.  0 )   GO TO 1441                                   M31.1435
 1480 DO 1481  I=1, NTOT                                                M31.1436
      IF ( KSSFRQ(I)  .EQ.  0 )   KSSFRQ(I) = -N13                      M31.1437
 1481 CONTINUE                                                          M31.1438
      IF ( IPRSUP  .GE.  2 )                                            M31.1439
     1 WRITE (LUNIT6, 1482)  ( KPSOUR(J), J=1, KCONST )                 M31.1440
 1482 FORMAT (  8H KPSOUR:,  20I5  )                                    M31.1441
      IF ( IPRSUP  .GE.  2 )                                            M31.1442
     1 WRITE (LUNIT6, 1484)  ( KSSFRQ(J), J=1, NTOT )                   M31.1443
 1484 FORMAT ( 43H FINAL  (KSSFRQ(J), J=1, NTOT)  FOLLOWS ...           M31.1444
     1   ,/,  ( 1X,  20I4 ) )                                           M31.1445
      DO 1497  K=1, KCONST                                              M31.1446
      IF ( KPSOUR(K)  .LT.  0 )   GO TO 1497                            M31.1447
      N16 = K                                                           M31.1448
      N2 = KPSOUR(K)                                                    M31.1449
      IF ( K  .EQ.  KCONST )   GO TO 1494                               M31.1450
      N7 = K + 1                                                        M31.1451
      DO 1492  J=N7, KCONST                                             M31.1452
      IF ( KPSOUR(J)  .NE.  N2 )   GO TO 1492                           M31.1453
      KPSOUR(N16) = -J                                                  M31.1454
      N16 = J                                                           M31.1455
 1492 CONTINUE                                                          M31.1456
 1494 KPSOUR(N16) = -K                                                  M31.1457
 1497 CONTINUE                                                          M31.1458
      DO 1502  K=1, KCONST                                              M31.1459
 1502 KPSOUR(K) = -KPSOUR(K)                                            M31.1460
      IF ( IPRSUP  .GE.  2 )                                            M31.1461
     1 WRITE (LUNIT6, 1482)  ( KPSOUR(J), J=1, KCONST )                 M31.1462
 9200 IF (KILL .NE. 222) GO TO 9202                                     M37.2953
      LSTAT(19) = 1462                                                  M37.2954
      LSTAT(14) = N5                                                    M37.2955
      LSTAT(15) = N10                                                   M37.2956
      N1 = N15                                                          M37.2957
      N2 = N20                                                          M37.2958
      GO TO 9204                                                        M37.2959
 9202 IF (KILL .NE. 223) GO TO 9204                                     M37.2960
      LSTAT(19) = 1471                                                  M37.2961
      LSTAT(14) = N9                                                    M37.2962
      BUS1 = BUS3                                                       M37.2963
      BUS2 = BUS4                                                       M37.2964
      LSTAT(15) = N18                                                   M37.2965
      LSTAT(16) = N19                                                   M37.2966
 9204 IF ( IPRSUP  .GE.  1 )                                            M37.2967
     1 WRITE (LUNIT6, 9207)                                             M31.1464
 9207 FORMAT (  15H EXIT "FRQCHK".  )                                   M31.1465
      RETURN                                                            M32.3215
      END                                                               M31.1466
      SUBROUTINE CXRED8(A,C,N,M)                                        M29.1587
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    ELIMINATION OF VARIABLES M+1,...N IN SYMMETRIC COMPLEX MATRIX WITHM29.1588
C)    A=REAL PART, C=IMAGINARY PART. A AND C ARE                        M29.1589
C)    STORED AS TRIANGLE (1 ELEMENT FOR 1.COLUMN,2 FOR 2.COLUMN ETC.).  M29.1590
C)    RESULT IS REDUCED MATRIX IN COLUMNS 1,...M IN CASE OF REDUCTION   M29.1591
C)    (M UNEQUAL 0) OR NEGATIVE INVERSE MATRIX IN COLUMNS 1,...N IN CASEM29.1592
C)    OF INVERSION (M=0).                                               M29.1593
      DIMENSION A(1),C(1),B(30),D(30)                                   M29.1594
      J = N + 1                                                         M29.1595
      W=1.0                                                             M29.1596
      IF(M.GT.0) W=-W                                                   M29.1597
      IJ=N*J/2                                                          M29.1598
    3 J=J-1                                                             M29.1599
      IF(J.EQ.M) RETURN                                                 M29.1600
      H1=A(IJ)                                                          M29.1601
      G1=C(IJ)                                                          M29.1602
      X=1.0/(H1*H1+G1*G1)                                               M29.1603
      H1=-H1*X                                                          M29.1604
      G1=G1*X                                                           M29.1605
      B(J)=H1                                                           M29.1606
      D(J)=G1                                                           M29.1607
      IJ=IJ-J                                                           M29.1608
      K=0                                                               M29.1609
      IK=0                                                              M29.1610
C                                   BEGIN K-LOOP                        M29.1611
    4 IK=IK+K                                                           M29.1612
      I1=IK+1                                                           M29.1613
      K=K+1                                                             M29.1614
      IF(K.GT.N) GO TO 3                                                M29.1615
      IF(K.LT.J) GO TO 9                                                M29.1616
      IF(W.LT.0.) GO TO 3                                               M29.1617
      IF(K.EQ.J) GO TO 7                                                M29.1618
      I=IK+J                                                            M29.1619
    5 H2=A(I)                                                           M29.1620
      G2=C(I)                                                           M29.1621
      B(K) = H2*H1-G2*G1                                                M29.1622
      D(K)=H2*G1+G2*H1                                                  M29.1623
C                                   BEGIN I-LOOP                        M29.1624
      I2=IK+K                                                           M29.1625
      L=0                                                               M29.1626
      DO 6 I=I1,I2                                                      M29.1627
      L=L+1                                                             M29.1628
      X=B(L)                                                            M29.1629
      Y=D(L)                                                            M29.1630
      A(I)=A(I)+X*H2-Y*G2                                               M29.1631
    6 C(I)=C(I)+X*G2+Y*H2                                               M29.1632
      IF(K.LT.J) GO TO 4                                                M29.1633
      I=IK+J                                                            M29.1634
      A(I)=B(K)                                                         M29.1635
      C(I)=D(K)                                                         M29.1636
      GO TO 4                                                           M29.1637
C                                   END I-LOOP                          M29.1638
    7 I=IJ                                                              M29.1639
      DO 8 L=1,J                                                        M29.1640
      I=I+1                                                             M29.1641
      C(I)=D(L)                                                         M29.1642
    8 A(I)=B(L)                                                         M29.1643
      GO TO 4                                                           M29.1644
C                                   END K-LOOP                          M29.1645
    9 I=IJ+K                                                            M29.1646
      GO TO 5                                                           M29.1647
      END                                                               M29.1648
      SUBROUTINE UMRENU (                                               M31.1467
     1 REACL, GPAR, FPAR, HIST,UMCURP, NODVO1, NODVO2, JCLTAC,          M33. 623
     2 JCLOUT,JTYPE,NODOM, JTMTAC, HISTOM, OMEGM, OMOLD, THETAM,        M33. 624
     3 REAMDU, REAMDS, FLXDS, FLXDR, REAMQU, FLXQS, FLXQR,              M31.1470
     4 JCDSAT, JCQSAT, FLXD, FLXQ, NPPAIR, ROTMOM, NCLD,                M31.1471
     5 NCLQ, JTQOUT, JOMOUT, JTHOUT, REAMQS, EPSOM, DCOEF,              M31.1472
     6 KCOIL, VOLTUM, ANGLUM, NODFUM, NODMUM,                           M31.1473
     7 KUMOUT, JUMOUT, UMOUTP )                                         M31.1474
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION  REACL(1), GPAR(1), FPAR(1), HIST(1), UMCURP(1)         M33. 625
      DIMENSION  NODVO1(1), NODVO2(1), JCLTAC(1), JCLOUT(1)             M31.1476
      DIMENSION  JTYPE(1), NODOM(1), JTMTAC(1), HISTOM(1)               M31.1477
      DIMENSION  OMEGM(1), OMOLD(1), THETAM(1)                          M31.1478
      DIMENSION  REAMDU(1), REAMDS(1), FLXDS(1), FLXDR(1)               M31.1479
      DIMENSION  REAMQU(1), FLXQS(1), FLXQR(1)                          M31.1480
      DIMENSION  JCDSAT(1), JCQSAT(1), FLXD(1), FLXQ(1)                 M31.1481
      DIMENSION  NPPAIR(1), ROTMOM(1), NCLD(1)                          M31.1482
      DIMENSION  NCLQ(1), JTQOUT(1), JOMOUT(1), JTHOUT(1)               M31.1483
      DIMENSION  REAMQS(1), EPSOM(1), DCOEF(1), KCOIL(1)                M31.1484
      DIMENSION  VOLTUM(1), ANGLUM(1), NODFUM(1), NODMUM(1)             M31.1485
      DIMENSION  KUMOUT(1), JUMOUT(1), UMOUTP(1)                        M31.1486
      DIMENSION  ZTHS3(3,3),ZTHS3I(3,3),FJM(4,4),FJV(4),GMAT(6,7)       M31.1487
      DIMENSION  DUMMA1(3,3), DUMMA2(3,3)                               M31.1488
      COMMON /UMLOCL/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,               M42.4965
     1 N12,N17,N18,N19,N20, D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,         M42.4966
     2 D12,D13,D14,D15,D16,D17,D18, LFIM3, LFIM4I, NCOMCL,              M42.4967
     3 NCOMUM, KCLD1, KCLQ1,KCLF, NMINUM,                               M42.4968
     4 LOPSS1, LOPSS2, LOPSS4, LOPSS8, LOPSS9, LOPSS10,slip             M42.4969
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'SPACE2.FOR;/NOLIST'                                           
      INCLUDE  'UMDECK.FOR;/NOLIST'                                           
C     KCONST IS THE ENTRY OF THE SOURCE TYPE 14 VECTOR                  M31.1493
C     NUMSUB IS THE ENTRY OF THE SUBNETWORK VECTOR                      M31.1494
C     ISUBEG(NUMSUB) IS THE FIRST NODE OF SUBNETWORK NUMSUB             M31.1495
C     N15 IS THE ENTRY OF THE COMPENSATION VECTOR                       M31.1496
C     KBUS(N15) AND MBUS(N15) ARE THE PAIR OF NODES OF                  M38.1654
C                            COMPENSATION VECTOR ENTRY N15.             M31.1498
C     KSUBUM STORES THE FIRST SUBNETWORK CONNECTED TO UM.               M31.1499
C     KSSFRQ(N) IS THE ENTRY KCONST OF THE TYPE 14 SOURCE TABLE         M33. 626
C       WHICH IS IN THE SAME SUBNETWORK AS THAT OF NODE N.              M37.2968
C     KPSOUR(KCONST) IS THE NEXT TYPE 14 SOURCE (IF ANY) WHICH          M33. 628
C       IS IN THE SAME SUBNETWORK AS TYPE 14 SOURCE KCONST.             M33. 629
C  **************** UMRENU CODING STRATEGY *********************        M37.2969
C   "FIRST PART" OF UMRENU CODE IS ENTERED TWICE IF LOAD-FLOW           M37.2970
C       IS REQUESTED (LOOPSS(10) = 1 OR 2). THIS IS NEEDED TO           M38.1655
C       LET THE LOAD-FLOW DO ITS WORK TO ADJUST THE SOURCES.            M38.1656
C       HOWEVER, IF UM TYPE-3 IS PRESENT, THE FIRST PART WILL           M38.1657
C       BE ENTERED ONE ADDITIONAL PASS PRIOR TO THE TWO                 M38.1658
C       MENTIONED PASSES IN ORDER TO FIND THE THEV PARAMETERS OF        M38.1659
C       THE UM TYPE-3 EXCITATION CIRCUITS (LOOPSS(10) = 3).             M38.1660
C   "SECOND PART" OF UMRENU CODE IS ENTERED TWICE IF ONE OF THE         M37.2977
C       UM IS AN INDUCTION MACHINE AND PREDICTION OPTION IS             M37.2978
C       REQUESTED (LOOPSS(9)=2) SINCE NORTON EQUIVALENT OF IND.         M37.2979
C       MACH. WOULD BE NEEDED, OTHERWISE IT IS ONLY PASSED ONCE.        M37.2980
C   "SLACK BUS" CALCULATIONS (2 PASSES) ARE CONDUCTED TO ADJUST         M37.2984
C       THE EXTERNAL EXCITATION AND MECHANICAL SOURCES SO AS TO         M37.2985
C       ACCOMMODATE THE UM CURRENTS FOUND IN PREVIOUS PASSES.           M37.2986
C   EMTP RESTORE FEATURE : BEFORE EACH REENTRANCE OF UMRENU AS          M37.2987
C       WELL AS AFTER PERMANENT EXIT OF UMRENU, THE EMTP                M37.2988
C       R,L,C BRANCHE ELEMENTS CREATED IN UMDATA ARE RESTORED           M37.2989
C       TO THE VALUES AS INITIALIZED IN UMDATA. HOWEVER, THE            M37.2990
C       BRANCHES CREATED IN UMRENU ARE NOT RESTORED, THEY WILL          M37.2991
C       MAINTAIN  VALUES AS INITIALIZED AT ANY DESIRED PASS             M37.2992
C       THROUGH UMRENU.                                                 M37.2993
C       THE VECTORS KPSOUR AND KSSFRQ WHICH ARE USED TO SCAN            M37.2994
C       THE CONNECTIVITY OF THE SUBNETWORKS, ARE ALSO RESTORED          M37.2995
C       BEFORE EACH REENTRANCE OF UMRENU. THUS ONLY THE                 M37.2996
C       NETWORK CONFIGURATION AS DETERMINED IN UMDATA WILL BE           M37.2997
C       OBSERVED BY THESE TWO VECTORS.                                  M37.2998
C   OVER11 IS ENTERED PERMANENTLY AFTER SLACK BUS CALCULATIONS.         M31.1504
C   LOOPSS(1) .GT. 0 = REQUEST TO GO BACK FROM OVER11 TO OVER8          M31.1505
C                      FOR MULTIPLE SS CALCULATIONS.                    M33. 630
C           (ON PERMANENT EXIT OF UMRENU, SET LOOPSS(1) = -1,           M33. 631
C            AND AFTER COMPLETION OF THIS LAST SS CALCULATION           M33. 632
C            IN OVER11, THEN THERE LOOPSS(1) IS SET TO 7766).           M33. 633
C   LOOPSS(2) = 3*NTOT = REQUEST TO SOLVE THEV VARIABLES, ELSE 0        M31.1507
C   LOOPSS(3) = USED TO STORE FIRST TYPE 14 SOURCE SET BY UM            M31.1508
C   LOOPSS(4) .GT. 0 = REQUEST TO SET SLACK BUSES AND UM                M38.1661
C                      STEADY-STATE EQNS ARE TOTALLY SKIPPED.           M36. 720
C             .EQ. 3 = UMRENU IS NEVER TO BE CALLED AGAIN               M31.1510
C   NOTE :  ALL BRANCHES CREATED IN UMRENU WILL BE WIPED                M33. 634
C           OUT AFTER TOTAL COMPLETION OF SS-CALCULATION.               M33. 635
C           HOWEVER, IF OPTION FOR NON-COMPENSATED POWER                M36. 721
C           COILS IS USED, CREATED BRANCHES ARE PERMANENT.              M36. 722
C   LOOPSS(5) IS TO STORE THE LAST BRANCH WHICH IS CREATED              M37.2999
C     CREATED PERMANENTLY BY UMRENU. TEMPORARY BRANCHES ARE NOT         M38.1662
C     INCLUDED TO LOOPSS(5)                                             M38.1663
C   LOOPSS(6) IS USED TO STORE KCONST BEFORE ADDITIONAL SOURCES         M38.1664
C      ARE CREATED DURING THE SLACK BUS CALCULATIONS                    M38.1665
C   LOOPSS(7) = USED TO STORE MISC. DATA PARAMETER "KSSOUT"             M31.1513
C   LOOPSS(8) WAS SET TO 1 IN UMDATA IF USER REQUESTED FOR              M36. 723
C     THE OPTION OF USING THE UM PREDICTION CODE (POWER COILS)          M36. 724
C   LOOPSS(9) IS SET TO 1 IN FIRST PART OF UMRENU IF LOOPSS(8)          M33. 638
C              WAS SET TO 1 AND IF ONE OF UM IS AN INDUCT. MACH.        M33. 639
C       THEN IN "SECOND PART" IF LOOPSS(9)=1, LOOPSS(9) IS SET          M37.3005
C          TO 2 TO REQUEST IS FOR NORTON PAR CALCULATIONS OF            M37.3006
C          IND. MACH AND DUMMY RUN FOR SM IF SM ALSO PRESENT.           M37.3007
C          HOWEVER : LOOPSS(9) IS SET IMMEDIATELY TO 3 IF LOAD-         M37.3008
C          IS REQUESTED, THUS BYPASSING NORTON CALCULATIONS.            M37.3009
C       THEN AT NEXT PASS IF LOOPSS(9)=2, LOOPSS(9) IS SET TO 3         M37.3010
C         INDICATING FINAL PASS THRU "SECOND PART" OF UMRENU,           M37.3011
C         AND IN THIS PASS THE UM POWER RESISTANCES ARE PLACED          M36. 727
C         ON THE STATOR IF UM PREDICTION IS REQUESTED.                  M36. 728
C   LOOPSS(10) IS SET TO 1 IF ISTEP = -4567 , INDICATING REQUEST        M37.3012
C         FOR LOAD FLOW INTERFACING.                                    M37.3013
C         LOOPSS(10)=1 , PASS THROUGH "FIRST PART" FOR PREPARING        M38.1666
C                        UM LOAD-FLOW EQUIVALENT.                       M38.1667
C         LOOPSS(10)=2 , PASS THROUGH "FIRST AND SECOND PART"           M38.1668
C                        FOR PROCESSING LOAD-FLOW OUTPUT AND            M38.1669
C                        PREPARING SS-CALCULATIONS.                     M38.1670
C                        ON COMPLETION OF THIS PASS, LOOPSS(10)         M38.1671
C                        IS RESET TO 0 ON EXIT OF "FIRST PASS".         M38.1672
C         LOOPSS(10)=3 , IF LD-FLOW WITH UM TYPE-3 IS REQUESTED.        M38.1673
C                        THIS RESULTS IN AN ADDITIONAL PASS THRU        M38.1674
C                        UMRENU PRIOR TO THE PASSES MENTIONED AS        M38.1675
C                        PASSES WITH LOOPSS(10) = 1 AND 2.              M38.1676
C                        AFTER COMPLETION OF THIS PASS WITH             M38.1677
C                        LOOPSS(10)=3, WE RESET LOOPSS(10) TO 1         M38.1678
C   LOOPSS(11) IS USED FOR PASSING "NEXT" IN OVER 5,6,7,8.              M37.3017
C   LOOPSS(12) IS USED FOR TEMPORARY STORAGE OF LOOPSS(5) IF            M37.3018
C      "FIRST PART" NEEDS TO BE ENTERED TWICE                           M37.3019
C   ISTART .EQ. 0 = RUN HAS NOT GONE THROUGH SECOND PART CODE           M31.1514
C      ISTART IS USED TO COUNT THE NUMBER OF UMRENU PASSES.             M38.1679
C   INITUM .EQ. 0 = NO STEADY STATE INITIALIZATION                      M31.1515
C               1 = USER'S REQUEST FOR SS INITIALIZATION                M31.1516
C   HIST(KCL) IS USED TEMPORARILY IN FIRST AND SECOND PART OF           M37.3020
C        UMRENU TO STORE ADDRESS IBR OF POWER RESIST. BRANCHE.          M37.3021
C   HIST(KCL+1) IS BORROWED TO STORE NUMSUB AS NEEDED FOR LD-           M38.1680
C        FLOW PROCESSING WITH UM TYPE-3. IT IS RELEASED AFTER           M38.1681
C        UMRENU IS REENTERED WHEN THE LD-FLOW IS COMPLETED.             M38.1682
C   HIST(KCL+2) IS BORROWED TO STORE KCONST OF POWER COIL SOURCE        M38.1683
C        IF THE LOAD-FLOW INVOLVES A UM TYPE-3 .                        M38.1684
C   FPAR(KCL) IS USED TO STORE ADDRESS IBR OF LEAKAGE                   M37.3022
C        INDUCTANCES (INITIALIZED IN UMDATA).                           M37.3023
C   FPAR(KCL+1) IS USED TO STORE FIRST SOURCE KCONST WHICH              M37.3024
C      REPRESENTS AN IM EXCITATION COIL (INITILIZED IN UMDATA).         M37.3025
C   FPAR(KCL+2) IS USED TO STORE IT ADDRESS OF THE SPEED                M39.1334
C      CAPACITOR (INITIALIZED IN UMDATA)                                M39.1335
C   FPAR(KCL+3) WAS INITIALIZED IN UMDATA AND IS - KCONST OF            M38.1685
C      THE EXCITER TORQUE SOURCE.                                       M38.1686
C   FPAR(KCL+4) AND FPAR(KCL+5) ARE BOTH INITIALIZED IN UMDATA          M38.1687
C      AND USED TO STORE RESP THE EXCITER MASS AND FIELD NODES.         M38.1688
C   JCLTAC(KCL AND KCL+1) WILL BE BORROWED TO STORE                     M38.1689
C      RESPECTIVELY 2 POTENTIAL UM'S CONNECTED TO THE SAME              M38.1690
C      MECHANICAL NETWORK AS THE UM WITH KCOIL(JM)=KCL                  M38.1691
C   JTMTAC(JM) WAS INITIALIZED IN UMDATA TO THE NEGATIVE VALUE          M39.1336
C      OF THE NODE NR OF THE SPEED CAPACITOR (THUS IF INITUM=1).        M39.1337
C      IT WILL BE RESET TO ZERO HERE AFTER ITS USE.                     M39.1338
C.  ENTRANCE CODE OF UMRENU ************************************        M31.1517
  300 IF (INITUM .EQ. 0) GO TO 305                                              
      IF (ISTEP .EQ. -4567) LOOPSS(10) = 1                              M39.1340
      LOOPSS(1) = 1                                                             
  305 LFIM3 = 0                                                                 
      IF ( IPRSUP  .GE. 1 )                                             M31.1519
     1 WRITE (LUNIT6,307) NUMUM,NCLTOT,NUMOUT,ISTART,KCONST,IBR,        M37.3027
     2 LOOPSS(1),LOOPSS(2),LOOPSS(4),LOOPSS(8),LOOPSS(9),               M37.3028
     3 LOOPSS(10)                                                       M37.3029
  307 FORMAT ( /,   18H BEGIN  "UMRENU" .,                              M37.3030
     1  48H   NUMUM  NCLTOT  NUMOUT  ISTART  KCONST     IBR,            M37.3031
     2  49H  LOPSS1  LOPSS2  LOPSS4  LOPSS8  LOPSS9  LOPSS10            M38.1699
     3   ,/, 18X,11I8,1X,I8)                                            M37.3033
      IF (LOOPSS(4) .GT. 0) GO TO 13100                                 M38.1700
C.  ....... CODE FOR UM STEADY-STATE INITIALIZATION ............        M31.1525
      CALL UMRNU2(  REACL,GPAR,FPAR, HIST,UMCURP,NODVO1,                M42.4970
     2 JTYPE,NODOM, JTMTAC,THETAM, IMACH,REAMDU, REAMQU,NODVO2,         M42.4971
     3 NPPAIR,ROTMOM, NCLD,JCLTAC, KCL,NCLQ, EPSOM,DCOEF,               M42.4972
     4 KCOIL,VOLTUM, ANGLUM,NODFUM, NODMUM, UMOUTP )                    M42.4973
      IF (ISTART .GT. 0) GOTO 7000                                      M42.4974
      IF (INITUM .EQ. 0) GOTO 14000                                     M42.4975
      IF (LFIM3 .EQ. 1) GOTO 7000                                       M42.4976
C  NOW GO OUT OF MODULE TO CALCULATE CURRENTS FOR UM POWER COILS        M31.1716
 2400 GO TO 13500                                                       M33. 684
C.  SECOND PART OF CODE : UMSSEQ = STEADY STATE EQUATIONS ******        M31.1718
C   THIS PART OF CODE IS ENTERED ONLY IF ISTART .GE. 1                  M31.1719
 7000 NCLOUT = LOOPSS(3) + 1                                            M31.1720
C  STORING LOOPSS(9) IN CASE OF UM TYPE-3 PRESENCE :                    M38.2026
      IF (LFIM3 .NE. 1) GO TO 7060                                      M38.2027
      LPSS9 = LOOPSS(9)                                                 M38.2028
      LOOPSS(9) = 2                                                     M38.2029
      GO TO 7090                                                        M38.2030
C  NORMAL CONTROL OF LOOPSS(9) :                                        M38.2031
 7060 IF (LOOPSS(9) .EQ. 2) LOOPSS(9) = 3                               M38.2032
      IF (LOOPSS(9) .EQ. 1) LOOPSS(9) = 2                                       
C.  START MACHINE DO-LOOP **************************************        M31.1721
 7090 DO 13050 JM = 1,NUMUM                                             M37.3417
      KCL = KCOIL(JM)                                                   M37.3418
      NUMIBR = HIST(KCL)                                                M37.3419
      LFIM3I = 0                                                                
      IF (LFIM3 .NE. 1) GO TO 7098                                      M38.2034
      IF (JTYPE(JM) .EQ. 3) LFIM3I = 1                                  M38.2035
      IF (LFIM3I .EQ. 0) GO TO 13050                                            
      IF (IPRSUP .GE. 1) WRITE(LUNIT6,7096) JM                          M38.2037
 7096 FORMAT(/,38H *************************************,               M38.2038
     1         42H NORTON EQUIVALENT OF UM TYPE-3 EXCITATION,           M38.2039
     2         36H CIRCUITS FOR LD-FLOW WITH UM NUMBER,I5,1H:)          M38.2040
      IF (IPRSUP .GE. 1) WRITE (LUNIT6,7097)                            M38.2041
 7097 FORMAT(20H *******************,5X,3HIBR,8X,6HTR(IT),              M38.2042
     1        8X,6HTX(IT),2X,6HKCONST,9X,5HSFREQ,9X,5HCREST,            M38.2043
     2        9X,5HTIME1,9X,5HTSTOP)                                    M38.2044
      GO TO 8002                                                        M38.2045
 7098 IF (IPRSUP .GE. 1) WRITE (LUNIT6,7100) JM                         M38.2046
 7100 FORMAT(/,38H *************************************,               M37.3420
     1         24H CHANGES TO CREATED EMTP,                             M37.3421
     2         42H ELEMENTS FOR STEADY-STATE CALCULATIONS OF,           M36. 810
     3         10H UM NUMBER,I4,2H :)                                   M36. 811
      IF (IPRSUP .GE. 1) WRITE (LUNIT6,7110)                            M36. 812
 7110 FORMAT(20H ********** CHANGED:,5X,3HIBR,8X,6HTR(IT),              M37.3422
     1        8X,6HTX(IT),2X,6HKCONST,9X,5HSFREQ,9X,5HCREST,            M36. 814
     2        9X,5HTIME1,9X,5HTSTOP)                                    M36. 815
C  IN CASE OF SM TYPE-59 DATA INPUT                                     M32.3239
      NTYP59 = 0                                                        M32.3240
      IF (JTYPE(JM) .NE. 13) GO TO 8002                                 M32.3241
      NTYP59 = 1                                                        M32.3242
      JTYPE(JM) = 1                                                     M32.3243
      INPUST = INPU                                                     M32.3244
      INPU = 0                                                          M32.3245
 8002 IMACH = 1                                                         M32.3246
      IF (JTYPE(JM) .LT. 3) IMACH = 0                                   M31.1724
      IF (JTYPE(JM) .GT. 7) IMACH = 0                                   M31.1725
      IF (IMACH .EQ. 1) SLIP = VOLTUM(JM)/100.0                         M31.1726
      ZTHRR = 0.0                                                       M33. 688
      ZTHRI = 0.0                                                       M33. 689
      DO 8003 N1 = 1,40                                                 M31.1727
      VINP(N1) = 0.0                                                    M31.1728
      ZTHEVS(N1) = 0.0                                                  M31.1729
 8003 UMCUR(N1) = 0.0                                                   M31.1730
      DO 8004 N1 = 1,3                                                  M31.1731
      DO 8004 N2 = 1,3                                                  M31.1732
      ZTHS3(N1,N2) = 0.0                                                M31.1733
 8004 ZTHS3I(N1,N2) = 0.0                                               M31.1734
C  NO SOURCE INITIALIZATION FOR SM IF NORTON CALCULATIONS ARE           M37.3423
C    REQUESTED FOR IM (THIS IS OF COURSE FOR MULTI-MACH CASE):          M37.3424
      IF (LOOPSS(9) .NE. 2) GO TO 8009                                  M37.3425
      IF (IMACH .NE. 0) GO TO 8009                                      M37.3426
 8005 DO 8008 N1 = 1,3                                                  M33. 694
      N2 = N1 + KCOIL(JM) - 1                                           M31.1737
      IF (NODVO1(N2) .EQ. NODVO2(N2)) GO TO 8008                        M33. 695
      IF (NODVO1(N2) .NE. 1) NCLOUT = NCLOUT + 1                        M31.1739
      IF (NODVO2(N2) .NE. 1) NCLOUT = NCLOUT + 1                        M31.1740
 8008 CONTINUE                                                          M33. 696
 8009 DO 8010 N1 = 1,10                                                 M33. 697
 8010 CON(N1) = 0.0                                                     M31.1743
      N1 = JTYPE (JM)                                                   M31.1744
      IF (N1 .GT. 12) GO TO 14000                                       M31.1745
      GO TO (8011,8012,8011,8011,8012,8013,8013,8014,8014,              M31.1746
     V8014,8014,8014), N1                                               M31.1747
 8011 CON(3) = 1.0                                                      M31.1748
      GO TO 8020                                                        M31.1749
 8012 CON(2) = 1.0                                                      M31.1750
      GO TO 8020                                                        M31.1751
 8013 CON(1) = 1.0                                                      M31.1752
      GO TO 8020                                                        M31.1753
 8014 CON(8) = 1.0                                                      M31.1754
 8020 SELTAT = ROTMOM(JM)                                               M37.3427
      IF (INPU .EQ. 1) SELTAT = 1.0                                     M31.1756
      IF (JTYPE(JM) .LE. 7) GO TO 8100                                  M31.1757
      IF (JTYPE(JM) .GT. 12) GO TO 8100                                 M31.1758
      SELTAT = OMEGM(JM) * NPPAIR(JM)                                   M31.1759
C.  COIL MARKINGS                                                       M31.1760
 8100 KCLD1 = KCL + 3                                                   M37.3428
      KCLDE = KCLD1 + NCLD(JM) - 1                                      M31.1763
      KCLQ1 = KCLDE + 1                                                 M31.1764
      KCLQE = KCLQ1 + NCLQ(JM) - 1                                      M31.1765
      KCLE = KCL + 2 + NCLD(JM) + NCLQ(JM)                              M37.3429
      NCL = 3 + NCLD(JM) + NCLQ(JM)                                     M31.1767
      IF (JTYPE(JM) .EQ. 4) KCLE = KCLE + 1                             M31.1768
      IF (JTYPE(JM) .EQ. 4) NCL = NCL + 1                               M31.1769
C.  SM AND DM EQUATIONS CODE ***********************************        M31.1770
C.  INPUT INTERFACE ELECT NETW WITH SM AND DM POWER SIDE *******        M31.1771
      IF (IMACH .EQ. 1) GO TO 10000                                     M31.1772
C  MAKE LEAKAGE INDUCTANCE ZERO FOR THIS NEXT ROUND OF SS               M36. 817
C   CALCULATION IF LOOPSS(9)=2 SINCE OTHERWISE IT WOULD BE              M37.3430
C   RESTORED TO VALUES OF UMDATA (SEE COMMENTS NEAR FORTRAN             M37.3431
C   STMT 2070)                                                          M37.3432
      IF (LOOPSS(9) .NE. 2) GO TO 8204                                  M37.3433
      D1 = EPSILN * 1.0D+3                                              M37.3434
      IF (XOPT .NE. 0.0) D1 = EPSILN * TWOPI * XOPT                     M38.2047
      DO 8203 N10 = 1,3                                                 M36. 821
      N1 = KCL - 1 + N10                                                M37.3436
      IF (NODVO1(N1) .EQ. NODVO2(N1)) GO TO 8203                        M36. 823
      N11 = FPAR(KCL) + N10 - 1                                                 
      N2 = - NR(N11)                                                            
      IF (N2 .LT. 0) N2 = - N2                                          M37.3439
      TX(N2) = D1                                                       M36. 825
      IF (IPRSUP .GE. 1)                                                M37.3440
     1 WRITE(LUNIT6,8860) N11,TR(N2),TX(N2)                             M37.3441
 8203 CONTINUE                                                          M36. 826
      GO TO 9045                                                                
 8204 D1 = ANGLUM(JM) * TWOPI/360.0                                     M36. 828
      DUMVEC(1) = D1 * CON(3)                                           M31.1774
      DUMVEC(2) = (DUMVEC(1) - TWOPI/3.0) * CON(3)                      M31.1775
      DUMVEC(2) = DUMVEC(2) + D1 * CON(2)                               M31.1776
      DUMVEC(3) = (DUMVEC(1) + TWOPI/3.0) * CON(3)                      M31.1777
      DUMVEC(3) = DUMVEC(3) + (DUMVEC(2) + TWOPI/4.0)*CON(2)            M31.1778
      D2 = EPSILN                                                       M36. 829
      N1 = NODVO1(KCL+2)                                                M36. 830
      N2 = NODVO2(KCL+2)                                                M36. 831
      IF (N1 .NE. 1 .AND. N2 .NE. 1) D2 = EPSILN * 1.0D+3               M36. 832
      DO 8210 N10 = 1,3                                                 M31.1779
      D1 = DUMVEC(N10)                                                  M31.1780
      N11 = N10 + KCL - 1                                               M31.1781
      N1 = NODVO1(N11)                                                  M31.1782
      N1 = NORDER(N1)                                                   M31.1783
      N2 = NODVO2(N11)                                                  M31.1784
      N2 = NORDER(N2)                                                   M31.1785
      VINP(N10) = 0.0                                                   M31.1786
      VINP(N10 + 20) = 0.0                                              M31.1787
      UMCUR(N10) = 0.0                                                  M31.1788
      UMCUR(N10+20) = 0.0                                               M31.1789
      IF (N1 .EQ. N2) GO TO 8210                                        M31.1790
      VINP(N10) = VOLTUM(JM) * COSZ(D1)                                 M31.1791
      VINP(N10+20) = VOLTUM(JM) * SINZ(D1)                              M31.1792
      UMCUR(N10) = VINP(N10) - SOLR(N1) + SOLR(N2)                      M31.1793
      UMCUR(N10) = UMCUR(N10)/D2                                        M36. 833
      UMCUR(N10+20) = VINP(N10+20) - SOLI(N1) + SOLI(N2)                M31.1795
      UMCUR(N10+20) = UMCUR(N10+20)/D2                                  M36. 834
 8210 CONTINUE                                                          M31.1797
 8300 IF (CON(1) .EQ. 1) GO TO 8800                                     M33. 712
      IF (CON(8) .EQ. 1) GO TO 8800                                     M33. 713
      D1 = VINP(1) + VINP(2) + VINP(3)                                  M33. 714
      D2 = VINP(21) + VINP(22) + VINP(23)                               M33. 715
      D1 = D1 * D1                                                      M33. 716
      D1 = D1 + D2 * D2                                                 M33. 717
      D1 = SQRTZ(D1)                                                    M33. 718
      IF (D1 .LT. EPSILN) GO TO 8800                                    M33. 719
      WRITE (LUNIT6,8310) JM                                            M33. 720
 8310 FORMAT( /, 36H WARNING : UMBALANCED ELECTRIC POWER,               M33. 721
     1           28H NETWORK ELEMENTS OR SOURCES,                       M33. 722
     2           18H CONNECTED TO UM -, I3, 16H.   STEADY-STATE,        M33. 723
     3           28H SOLUTION WILL NOT BE EXACT.,/)                     M33. 724
C.  CALCULATION OF POWER SIDE POS SEQUENCE INPUT VOLT AND CURR:         M31.1798
 8800 D10 = CON(1) + CON(4)                                             M33. 725
      VINP(1) = CON(3)*VINP(1)+CON(2)*VINP(2)+D10*VINP(3)               M33. 726
      VINP(21) = CON(3)*VINP(21)+CON(2)*VINP(22)+D10*VINP(23)           M33. 727
      UMCUR(1) = CON(3)*UMCUR(1)+CON(2)*UMCUR(2)+D10*UMCUR(3)           M33. 728
      UMCUR(21) = CON(3)*UMCUR(21)+CON(2)*UMCUR(22)                     M33. 729
      UMCUR(21) = UMCUR(21) + D10*UMCUR(23)                             M33. 730
C.  INCLUDING MACH RESISTANCES AND RESTORE LEAKAGE INDUCTANCES          M36. 836
C       TO POWER NETWORK; IN CASE OF COMPENSATED POWER COILS            M37.3443
C       THE RESISTANCES ARE KEPT IN THE UM MODULES.                     M37.3444
      D19 = VINP(1)                                                     M36. 840
      D20 = VINP(21)                                                    M36. 841
      D17 = REACL(KCL+2)                                                M36. 842
      IF (D17 .GT. REACL(KCL+1)) D17 = REACL(KCL+1)                     M36. 843
      IF (JTYPE(JM) .GT. 7) GO TO 8840                                  M36. 844
      D18 = D17 * SELTAT                                                M36. 845
      D19 = D19 - D18 * UMCUR(21)                                       M36. 846
      D20 = D20 + D18 * UMCUR(1)                                        M36. 847
 8840 N11 = - NR(NUMIBR)                                                M37.3445
      IF (N11 .LT. 0) N11 = - N11                                       M37.3446
      D1 = 1.0/TR(N11)                                                  M37.3447
      IF (LOOPSS(8) .EQ. 1) D1 = GPAR(KCL+2)                            M37.3448
      D11 = D19*D1 + UMCUR(1)                                           M37.3449
      D12 = D20*D1 + UMCUR(21)                                          M37.3450
      D10 = D11*D11 + D12*D12                                           M33. 737
      D10 = SQRTZ(D10)                                                  M33. 738
      D1 = D12/D11                                                      M33. 739
      D1 = ATANZ(D1)                                                    M33. 740
 8850 IF (D11 .LT. 0.0) D1 = D1 + TWOPI/2.0                             M33. 741
      D2 = CON(2)*D1 + CON(3)*(D1 - TWOPI/3.0)                          M33. 742
      D3 = CON(2)*(D1 + TWOPI/4.0) + CON(3)*(D1 + TWOPI/3.0)            M33. 743
      D3 = D3 + CON(1)*D1                                               M33. 744
      DO 8510 N1 = 1,3                                                  M33. 745
      N2 = N1 + KCL - 1                                                 M33. 746
      N11 = 3 * (JM - 1)                                                M33. 747
      N13 = 3 * (NUMUM + JM - 1)                                        M33. 748
      IF (NODVO1(N2) .EQ. NODVO2(N2)) GO TO 8510                        M33. 749
      IF (LOOPSS(8) .NE. 1) GO TO 8870                                  M37.3451
      N12 = NUMIBR - 3 + N1                                             M37.3452
      N10 = - NR(N12)                                                   M37.3453
      IF (N10 .LT. 0) N10 = - N10                                       M37.3454
      TR(N10) = 1.0/GPAR(KCL+2)                                         M37.3455
      IF (IPRSUP .GE. 1)                                                M37.3456
     1 WRITE(LUNIT6,8860) N12,TR(N10),TX(N10)                           M37.3457
 8860 FORMAT(20H ********** CHANGED:,2X,I6,2E14.5)                      M36. 852
 8870 D15 = D1                                                          M37.3458
      IF (N1 .EQ. 2) D15 = D2                                           M33. 753
      IF (N1 .EQ. 3) D15 = D3                                           M33. 754
      IF (NODVO1(N2) .EQ. 1) GO TO  8500                                M33. 755
      CREST(NCLOUT) = D10                                               M33. 756
      TIME1(NCLOUT) = D15                                               M33. 757
      IF (IPRSUP .GE. 1)                                                M36. 853
     1 WRITE(LUNIT6,8862) NCLOUT,SFREQ(NCLOUT),CREST(NCLOUT),           M36. 854
     2                    TIME1(NCLOUT),TSTOP(NCLOUT)                   M36. 855
 8862 FORMAT(20H ********** CHANGED:,38X,I6,4E14.5)                     M36. 856
      NCLOUT = NCLOUT + 1                                               M33. 758
 8500 IF (NODVO2(N2) .EQ. 1) GO TO 8505                                 M33. 759
      CREST(NCLOUT) = - D10                                             M33. 760
      TIME1(NCLOUT) = D15                                               M33. 761
      IF (IPRSUP .GE. 1)                                                M36. 857
     1 WRITE(LUNIT6,8862) NCLOUT,SFREQ(NCLOUT),CREST(NCLOUT),           M36. 858
     2                    TIME1(NCLOUT),TSTOP(NCLOUT)                   M36. 859
      NCLOUT = NCLOUT + 1                                               M33. 762
 8505 N3 = FPAR(KCL) + N1 - 1                                           M37.3459
      N4 = - NR(N3)                                                     M37.3460
      IF (N4 .LT. 0) N4 = - N4                                          M37.3461
      TX(N4) = D17 * 1.0D+3                                             M37.3462
      IF (XOPT .NE. 0.0) TX(N4) = D17 * TWOPI * XOPT                    M38.2048
      IF (IPRSUP .GE. 1)                                                M36. 863
     1 WRITE(LUNIT6,8860) N3,TR(N4),TX(N4)                              M37.3464
 8510 CONTINUE                                                          M33. 765
C   UM DOMAIN POS SEQUENCE VOLT AND CURRENT :                           M33. 767
 9000 D10 = CON(3)*SROOT3 + CON(2)*SROOT2 + CON(1) + CON(8)             M33. 768
      VINP(1) = D10 * VINP(1)                                           M33. 769
      VINP(21) = D10 * VINP(21)                                         M33. 770
      UMCUR(1) = D10 * UMCUR(1)                                         M33. 771
      UMCUR(21) = D10 * UMCUR(21)                                       M33. 772
      IF (JCDSAT(JM) .NE. 5) GO TO 9002                                 M36. 868
      REAMQU(JM) = REAMDU(JM)                                           M36. 869
      REAMQS(JM) = REAMDS(JM)                                           M36. 870
      FLXQR(JM) = FLXDR(JM)                                             M36. 871
      FLXQS(JM) = FLXDS(JM)                                             M36. 872
 9002 CDSAT = 0.0                                                               
      CQSAT = 0.0                                                               
      FLXDA = 0.0                                                       M36. 875
      FLXQA = 0.0                                                       M36. 876
      FLXDRR = 0.0                                                      M36. 877
      FLXQRR = 0.0                                                      M36. 878
      SFLXD = 1.0                                                               
      SFLXQ = 1.0                                                               
      N2 = 0                                                            M36. 879
C.  DM EXCITATION CODE *****************************************        M31.1836
      D2 = 1.0/GPAR(KCL+2)                                              M36. 880
      THETAM(JM) = TWOPI/(4.0*NPPAIR(JM))                               M31.1838
      VINP(1) = VINP(1) + D2*UMCUR(1)                                   M31.1839
C   STORING UMCUR AND VINP FROM EMTP NETWORK :                          M36. 881
 9004 D11 = UMCUR(1)                                                    M36. 882
      D12 = UMCUR(21)                                                   M36. 883
      D13 = VINP(1)                                                     M36. 884
      D14 = VINP(21)                                                    M36. 885
      UMCUR(2) = 0.0                                                    M31.1840
      UMCUR(3) = D11                                                    M36. 886
      IF (JTYPE(JM) .GT. 7) GO TO 9006                                  M36. 887
C.  SM EXCITATION CODE *****************************************        M31.1843
      D1 = (1.0 - CQSAT)*REAMQU(JM) + CQSAT*REAMQS(JM)                  M36. 888
      D1 = (D1 + REACL(KCL+2)) * SELTAT                                 M36. 889
      D13 = D13 - D1*D12                                                M36. 890
      D14 = D14 + D2*D12 + D1*D11                                       M36. 891
      D3 = D14/D13                                                      M36. 892
      D3 = ATANZ(D3)                                                            
      IF (D13 .LT. 0.0) D3 = D3 + TWOPI/2.0                             M36. 893
      D8 = (D13*D13 + D14*D14)/2.0                                      M38.2054
      D8 = SQRTZ(D8)                                                            
      D7 = CQSAT * SFLXQ * FLXQA * SELTAT                               M38.2056
      D7 = D7 + (1.0 - CQSAT) * SFLXQ * FLXQRR * SELTAT                 M38.2057
      IF (D7 .EQ. 0.0) GO TO 9005                                       M38.2058
      D6 = D8*D8 - D7*D7                                                M38.2059
      D6 = SQRTZ(D6)                                                    M38.2060
      D6 = - D7/D6                                                      M38.2061
      D6 = ATANZ(D6)                                                    M38.2062
      D3 = D3 - D6                                                              
      D8 = D8 * COSZ(D6)                                                M38.2064
 9005 D4 = COSZ(D3)                                                     M38.2065
      D5 = SINZ(D3)                                                     M31.1850
      THETAM(JM) = (D3 + TWOPI/4.0)/NPPAIR(JM)                          M31.1851
      UMCUR(3) = (D4*D11 + D5*D12)/SROOT2                               M36. 898
      UMCUR(2) = (D4*D12 - D5*D11)/SROOT2                               M36. 899
 9006 D1 = (1.0 - CDSAT)*REAMDU(JM) - (1.0 - CQSAT)*REAMQU(JM)          M36. 900
      D1 = D1 + CDSAT*REAMDS(JM) - CQSAT*REAMQS(JM)                     M36. 901
      D1 = D1 * UMCUR(2)                                                M36. 902
      D3 = (REACL(KCL+1) - REACL(KCL+2)) * UMCUR(2)                     M36. 903
      D1 = D1 + D3                                                      M36. 904
      UMCUR(4) = D8/SELTAT - CDSAT*SFLXD*FLXDA - D1                     M38.2066
      UMCUR(4) = UMCUR(4) - (1.0 - CDSAT)*SFLXD*FLXQRR                  M38.2067
      D3 = (1.0 - CDSAT)*REAMDU(JM) + CDSAT*REAMDS(JM)                  M36. 906
      UMCUR(4) = UMCUR(4)/D3                                            M36. 907
      VINP(4) = UMCUR(4)/GPAR(KCLD1)                                    M31.1859
      IF (N2 .GT. 0) GO TO 9015                                         M36. 908
C   SATURATION CHECK :                                                  M36. 909
      N1 = JCDSAT(JM) + JCQSAT(JM)                                      M36. 910
      IF (N1 .EQ. 0) GO TO 9015                                         M36. 911
      IF (INPU .EQ. 2) GO TO 9009                                       M38.2068
      FLXDR(JM) = 0.0                                                   M38.2069
      FLXQR(JM) = 0.0                                                   M38.2070
      GO TO 9010                                                        M38.2071
C  IF REMANENT FLUX OPTION IS REQUESTED (INPU = 2) :                    M36. 916
 9009 IF (FLXDR(JM) .LT. 0.0) FLXDR(JM) = - FLXDR(JM)                   M36. 920
      IF (FLXQR(JM) .LT. 0.0) FLXQR(JM) = - FLXQR(JM)                   M36. 921
 9010 IF (JCDSAT(JM) .NE. 5) GO TO 9011                                 M36. 922
      FLXDRR = FLXDR(JM)                                                M36. 923
      FLXQRR = FLXDRR                                                   M36. 924
      GO TO 9015                                                        M38.2072
 9011 FLXDRR = FLXDR(JM)                                                M36. 928
      FLXQRR = FLXQR(JM)                                                M36. 929
 9015 D10 = UMCUR(2) + UMCUR(4)                                                 
      D15 = UMCUR(3)                                                            
      IF (N1 .EQ. 0) GO TO 89028                                        M38.2075
      IF (JCDSAT(JM) .EQ. 5) GO TO 9016                                 M38.2076
      IF (N2 .EQ. 0) GO TO 9020                                         M38.2077
      GO TO 89028                                                               
C  CODE FOR THE CASE OF TOTAL SATURATION (JCDSAT = 5) :                 M38.2079
 9016 CURMT = D10*D10 + D15*D15                                         M38.2080
      IF (CURMT .EQ. 0.0) GO TO 89028                                   M38.2081
      CURMT = SQRTZ(CURMT)                                              M36. 947
      D19 = D10 * FLXDS(JM)/CURMT                                       M36. 948
      D20 = D15 * FLXQS(JM)/CURMT                                       M36. 949
      IF (D19 .LT. 0.0) D19 = - D19                                     M36. 950
      IF (D20 .LT. 0.0) D20 = - D20                                     M36. 951
      IF (REAMDU(JM) .EQ. 0.0) GO TO 9018                               M38.2082
      D18 = REAMDS(JM)/REAMDU(JM)                                       M36. 953
      FLXDA = D19 + D18*(FLXDRR - D19)                                  M36. 954
 9018 FLXQA = D20 + D18*(FLXQRR - D20)                                  M38.2083
      IF (N2 .GT. 0) GO TO 89028                                        M38.2084
      GO TO 9027                                                                
C  CODE FOR THE CASE OF NON-TOTAL SATURATION :                          M38.2086
 9020 IF (JCDSAT(JM) .EQ. 0) GO TO 9024                                 M38.2087
      IF (REAMDU(JM) .EQ. 0.0) GO TO 9024                               M38.2088
      D11 = FLXDS(JM)/REAMDU(JM)                                        M36. 958
      D12 = - D11                                                       M36. 959
      IF (D10 .GT. D11) CDSAT = 1.0                                     M38.2089
      IF (D10 .LT. D12) CDSAT = 1.0                                     M38.2090
      FLXDA = (REAMDS(JM)/REAMDU(JM)) * (FLXDRR - FLXDS(JM))            M38.2091
      FLXDA = FLXDA + FLXDS(JM)                                         M38.2092
 9024 IF (JCQSAT(JM) .EQ. 0) GO TO 9027                                 M38.2093
      D16 = FLXQS(JM)/REAMQU(JM)                                        M36. 974
      D17 = - D16                                                       M36. 975
      IF (D15 .GT. D16) CQSAT = 1.0                                     M38.2094
      IF (D15 .LT. D17) CQSAT = 1.0                                     M38.2095
      FLXQA = (REAMQS(JM)/REAMQU(JM)) * (FLXQRR - FLXQS(JM))            M38.2096
      FLXQA = FLXQA + FLXQS(JM)                                         M38.2097
 9027 N2 = 1                                                            M38.2098
      IF (D10 .LT. 0.0) SFLXD = - 1.0                                   M38.2099
      IF (D15 .LT. 0.0) SFLXQ = - 1.0                                   M38.2100
      D1 = CDSAT + CQSAT                                                M38.2101
      IF (D1 .NE. 0.0) GO TO 9004                                               
C   CALCULATIONS OF MAIN FLUXES :                                       M38.2103
89028 IF (CDSAT .EQ. 1.0) GO TO 89030                                   M38.2104
      FLXD(JM) = REAMDU(JM) * D10 + SFLXD * FLXDRR                      M38.2105
      GO TO 89032                                                       M38.2106
89030 FLXD(JM) = REAMDS(JM) * D10 + SFLXD * FLXDA                       M38.2107
89032 IF (CQSAT .EQ. 1.0) GO TO 89034                                   M38.2108
      FLXQ(JM) = REAMQU(JM) * D15 + SFLXQ * FLXQRR                      M38.2109
      GO TO 9028                                                        M38.2110
89034 FLXQ(JM) = REAMQS(JM) * D15 + SFLXQ * FLXQA                       M38.2111
C.  OUTPUT FOR UM = SM AND DM :                                         M31.1860
 9028 UMCUR(1) = 0.0                                                    M36. 990
      IF (JTYPE(JM) .GT. 7) GO TO 9029                                  M36. 991
      OMEGM(JM) = SELTAT/NPPAIR(JM)                                     M31.1864
 9029 OMOLD(JM) = OMEGM(JM)                                             M36. 992
      DO 9030 N1 = 1,3                                                  M31.1866
      N2 = KCL - 1 + N1                                                 M31.1867
 9030 HIST(N2) = UMCUR (N1)                                             M31.1868
      HIST(KCLD1) = - UMCUR(4)                                          M31.1869
C   ZEROING OF INTERNAL UM LEAKAGE SINCE LEAKAGE INDUCTANCES            M37.3466
C        ARE MOVED TO THE EMTP NETWORK.                                 M37.3467
      D17 = REACL(KCL+2)                                                M37.3468
      IF (REACL(KCL) .GT. D17) REACL(KCL) = REACL(KCL) - D17            M37.3469
      REACL(KCL+1) = REACL(KCL+1) - D17                                 M37.3470
      REACL(KCL+2) = 0.0                                                M37.3471
      N1 = KCLD1 + 1                                                    M37.3472
      DO 9040 N3 = N1,KCLE                                              M31.1871
 9040 HIST(N3) = 0.0                                                    M31.1872
C.  OUTPUT INTERFACE OF SM AND DM EXCITATION WITH ELECT. NETW.          M31.1873
      VOLTUM(JM) = VINP(4)                                              M31.1874
      IF (NTYP59 .EQ. 1) VOLTUM(JM) = VINP(4)/DCOEF(JM)                 M32.3249
 9045 IF (NODVO1(KCL+3) .EQ. 1) GO TO 9050                              M33. 774
      IF (LOOPSS(9) .EQ. 2) GO TO 9046                                  M37.3473
      CREST(NCLOUT) = - UMCUR(4)                                        M31.1876
      IF (NTYP59 .EQ. 1) CREST(NCLOUT) = - UMCUR(4)*DCOEF(JM)           M32.3250
      IF (IPRSUP .GE. 1)                                                M36. 993
     1 WRITE(LUNIT6,8862) NCLOUT,SFREQ(NCLOUT),CREST(NCLOUT),           M36. 994
     2                    TIME1(NCLOUT),TSTOP(NCLOUT)                   M36. 995
 9046 NCLOUT = NCLOUT + 1                                               M33. 776
 9050 IF (NODVO2(KCL+3) .EQ. 1) GO TO 9060                              M31.1878
      IF (LOOPSS(9) .EQ. 2) GO TO 9052                                  M37.3474
      CREST(NCLOUT) = + UMCUR(4)                                        M31.1879
      IF (NTYP59 .EQ. 1) CREST(NCLOUT) = + UMCUR(4)*DCOEF(JM)           M32.3251
      IF (IPRSUP .GE. 1)                                                M36. 996
     1 WRITE(LUNIT6,8862) NCLOUT,SFREQ(NCLOUT),CREST(NCLOUT),           M36. 997
     2                    TIME1(NCLOUT),TSTOP(NCLOUT)                   M36. 998
 9052 NCLOUT = NCLOUT + 1                                               M33. 778
 9060 IF (LOOPSS(9) .EQ. 2) GO TO 12100                                 M39.1396
C  IN CASE OF SM TYPE-59 AND EXCITER MASS IS PRESENT :                  M38.2113
      IF (NTYP59 .NE. 1) GO TO 12000                                    M38.2114
      IF (FPAR(KCL+3) .GE. 0.0) GO TO 12000                                     
      N3 = - FPAR(KCL+3)                                                        
      D1 = UMCUR(4) * UMCUR(4)                                          M38.2117
      CREST(N3) = - 2.0 * D1/(GPAR(KCLD1) * OMEGM(JM))                  M38.2118
      GO TO 12000                                                       M33. 780
C.  IM EQUATIONS CODE *****************************************         M31.1882
C.  INPUT INTERFACE ELECT NETW WITH IM POWER SIDE **************        M31.1883
C     NORTON=0 MEANS NO NORTON CALCULATIONS                             M33. 781
C     NORTON=1 MEANS CALCULATION OF I - NORTON                          M33. 782
C     NORTON=2 MEANS CALCULATION OF Y - NORTON                          M33. 783
C  NOTE : NORTON PARAMETER CALCULATIONS ARE ONLY PERFORMED              M36. 999
C         IN THE PASS WITH LOOPSS(9) = 2                                M36.1000
10000 NORTON = 0                                                        M33. 784
      IF (LOOPSS(9) .EQ. 2) NORTON = 1                                  M38.2119
10019 IF (NORTON .EQ. 1) GO TO 10080                                    M37.3511
      DO 10020 N10 = 1,3                                                M33. 787
      N11 = N10 + KCL - 1                                               M31.1885
      IF (NODVO1(N11) .EQ. NODVO2(N11)) GO TO 10020                     M31.1886
      N1 = NODVO1(N11)                                                  M31.1887
      N1 = NORDER(N1)                                                   M31.1888
      N2 = NODVO2(N11)                                                  M31.1889
      N2 = NORDER(N2)                                                   M33. 788
      DUMVEC(N10) = SOLR(N1) - SOLR(N2)                                 M31.1891
      DUMVEC(N10+3) = SOLI(N1) - SOLI(N2)                               M31.1892
10020 CONTINUE                                                          M31.1893
      IF (CON(1) .EQ. 1) GO TO 10024                                    M33. 789
      IF (CON(8) .EQ. 1) GO TO 10024                                    M33. 790
      IF (LOOPSS(9) .EQ. 2) GO TO 10024                                 M37.3512
10022 D1 = DUMVEC(1) + DUMVEC(2) + DUMVEC(3)                            M33. 793
      D2 = DUMVEC(4) + DUMVEC(5) + DUMVEC(6)                            M33. 794
      D1 = D1 * D1                                                      M33. 795
      D1 = D1 + D2 * D2                                                 M33. 796
      D1 = SQRTZ(D1)                                                    M33. 797
      IF (D1 .LT. EPSILN) GO TO 10024                                   M33. 798
      WRITE (LUNIT6,8310) JM                                            M33. 799
C   POS SEQUENCE REAL AND IMAGINARY POWER THEV VOLTAGES :               M31.1894
10024 VINP(1) = SROOT3*CON(3)*DUMVEC(1)+SROOT2*CON(2)*DUMVEC(2)         M33. 800
      VINP(1) = VINP(1) + CON(1)*DUMVEC(3)                              M31.1896
      VINP(21) = SROOT3*CON(3)*DUMVEC(4)+SROOT2*CON(2)*DUMVEC(5)        M31.1897
      VINP(21) = VINP(21) + CON(1)*DUMVEC(6)                            M31.1898
10028 IF (LOOPSS(8) .EQ. 1) GO TO 10080                                 M37.3523
      N10 = 0                                                           M37.3524
      N20 = 2                                                           M31.1902
      IF (NODVO1(KCL) .EQ. NODVO2(KCL)) N20 = 1                         M31.1903
10030 N1 = NODVO1(KCL+1)                                                M31.1904
      N1 = NORDER(N1) + N20*NTOT                                        M31.1905
      N2 = NODVO2(KCL+1)                                                M31.1906
      N2 = NORDER(N2) + N20*NTOT                                        M31.1907
C   POS SEQUENCE REAL AND IMAGINARY POWER THEV IMPEDANCES :             M31.1908
      ZTHRR = - ZTHRR + SOLR(N1) - SOLR(N2)                             M31.1909
      ZTHRI = - ZTHRI + SOLI(N1) - SOLI(N2)                             M31.1910
      IF (N10 .EQ. 1) GO TO 10040                                       M31.1911
      N20 = N20 + 1                                                     M31.1912
      N10 = 1                                                           M31.1913
      GO TO 10030                                                       M31.1914
10040 IF (CON(1) .NE. 1.0) GO TO 10080                                  M31.1915
      N1 = NODVO1(KCL+2)                                                M31.1916
      N1 = NORDER(N1)                                                   M31.1917
      N2 = NODVO2(KCL+2)                                                M31.1918
      N2 = NORDER(N2)                                                   M31.1919
      ZTHRR = - SOLR(N1+NTOT) + SOLR(N2+NTOT)                           M31.1920
      ZTHRI = - SOLI(N1+NTOT) + SOLI(N2+NTOT)                           M31.1921
C.  FORMATION OF CONSTANT SWITCH MATRIX PTHETA :                        M31.1922
10080 DO 10090 N1 = 1,3                                                 M38.2120
10090 PTHETA(3,N1) = 1.0/SROOT3                                         M31.1924
      PTHETA(1,1) = - 1.0/(SROOT2*SROOT3)                               M31.1925
      PTHETA(1,3) = SROOT2/SROOT3                                       M31.1926
      PTHETA(1,2) = PTHETA(1,1)                                         M31.1927
      PTHETA(2,1) = - 1.0/SROOT2                                        M31.1928
      PTHETA(2,2) = - PTHETA(2,1)                                       M31.1929
      PTHETA(2,3) = 0.0                                                 M31.1930
C.  INPUT INTERFACE IM EXCITATION AND ELECT.NETW. (THEV VARIABLES):     M31.1931
10100 N17 = KCLE - KCL - 2                                              M31.1932
      IF (NCL .GT. 6) N17 = 3                                           M31.1933
      DO 10110 N1 = 1,9                                                 M31.1934
10110 NDUM(N1) = 0                                                      M31.1935
      DO 10120 N1 = 1,N17                                               M31.1936
      N2 = KCL + 2 + N1                                                 M31.1937
      N3 = NODVO1(N2)                                                   M31.1938
      NDUM(N1) = NORDER(N3)                                             M31.1939
      N3 = NODVO2(N2)                                                   M31.1940
      NDUM(N1+3) = NORDER(N3)                                           M31.1941
      NDUM(N1+6) = NODVO1(N2) - NODVO2(N2)                              M31.1942
10120 CONTINUE                                                          M31.1943
      DO 10130 N1 = 1,N17                                               M31.1944
      N2 = NDUM(N1)                                                     M31.1945
      N3 = NDUM(N1+3)                                                   M31.1946
      N4 = N1 + 6                                                       M31.1947
      IF (NDUM(N4) .EQ. 0) GO TO 10130                                  M31.1948
      VINP(N1+3) = - SOLR(N2) + SOLR(N3)                                M31.1949
      VINP(N1+23) = - SOLI(N2) + SOLI(N3)                               M31.1950
      IF (SLIP .LT. 0.0) VINP(N1+23) = - VINP(N1+23)                    M35.1123
10130 CONTINUE                                                          M31.1951
      N20 = 1                                                           M31.1952
      DO 10140 N1 = 1,N17                                               M31.1953
      N2 = NDUM(N1) + N20 * NTOT                                        M31.1954
      N3 = NDUM(N1+3) + N20 * NTOT                                      M31.1955
      N5 = NDUM(N1) + NTOT                                              M31.1956
      N6 = NDUM(N1+3) + NTOT                                            M31.1957
      N7 = N1 + 6                                                       M31.1958
      N8 = NDUM(1) + N20 * NTOT                                         M31.1959
      N9 = NDUM(4) + N20 * NTOT                                         M31.1960
      IF (NDUM(N7) .EQ. 0) GO TO 10140                                  M31.1961
      N20 = N20 + 1                                                     M31.1962
      ZTHS3(N1,N1) = - SOLR(N2) + SOLR(N3)                              M31.1963
      ZTHS3I(N1,N1) = - SOLI(N2) + SOLI(N3)                             M31.1964
      IF (N17 .EQ. 1) GO TO 10200                                       M35.1124
      IF (N1 .EQ. 1) GO TO 10140                                        M31.1966
      IF (NDUM(7) .EQ. 0) GO TO 10140                                   M31.1967
      ZTHS3(1,N1) = - SOLR(N8) + SOLR(N9)                               M31.1968
      ZTHS3I(1,N1) = - SOLI(N8) + SOLI(N9)                              M31.1969
      ZTHS3(N1,1) = - SOLR(N5) + SOLR(N6)                               M31.1970
      ZTHS3I(N1,1) = - SOLI(N5) + SOLI(N6)                              M31.1971
10140 CONTINUE                                                          M31.1972
      IF (SLIP .GT. 0.0) GO TO 10146                                    M35.1125
      DO 10144 N1 = 1,N17                                               M35.1126
      DO 10142 N2 = 1,N17                                               M35.1127
10142 ZTHS3I(N1,N2) = - ZTHS3I(N1,N2)                                   M35.1128
10144 CONTINUE                                                          M35.1129
10146 IF (N17 .LT. 3) GO TO 10200                                       M35.1130
      N19 = 0                                                           M31.1974
      N20 = 1                                                           M31.1975
      N1 = NDUM(8) * NDUM(9)                                            M31.1976
      IF (N1 .EQ. 0) GO TO 10170                                        M31.1977
      IF (NDUM(7) .NE. 0) N20 = 2                                       M31.1978
      N1 = 3                                                            M31.1979
10150 N2 = NDUM(N1) + N20 * NTOT                                        M31.1980
      N3 = NDUM(N1+3) + N20 * NTOT                                      M31.1981
      IF (N19 .EQ. 1) GO TO 10160                                       M31.1982
      ZTHS3(3,2) = - SOLR(N2) + SOLR(N3)                                M31.1983
      ZTHS3I(3,2) = - SOLI(N2) + SOLI(N3)                               M31.1984
      N20 = N20 + 1                                                     M31.1985
      N19 = 1                                                           M31.1986
      N1 = 2                                                            M31.1987
      GO TO 10150                                                       M31.1988
10160 ZTHS3(2,3) = - SOLR(N2) + SOLR(N3)                                M31.1989
      ZTHS3I(2,3) = - SOLI(N2) + SOLI(N3)                               M31.1990
10170 IF (NCL .LE. 6) GO TO 10200                                       M35.1131
      N17 = KCLD1 + 3                                                   M31.1992
      DO 10180 N1 = N17,KCLE                                            M31.1993
      N2 = NODVO1(N1)                                                   M31.1994
      N2 = NORDER(N2)                                                   M31.1995
      N4 = N2 + NTOT                                                    M31.1996
      N3 = NODVO2(N1)                                                   M31.1997
      N3 = NORDER(N3)                                                   M31.1998
      N5 = N3 + NTOT                                                    M31.1999
      N10 = N1 - KCL + 1                                                M31.2000
      IF (NODVO1(N1) .EQ. NODVO2(N1)) GO TO 10180                       M31.2001
      ZTHEVS(N10) = - SOLR(N4) + SOLR(N5)                               M31.2002
      ZTHEVS(N10+20) = - SOLI(N4) + SOLI(N5)                            M31.2003
      VINP(N10) = - SOLR(N2) + SOLR(N3)                                 M31.2004
      VINP(N10+20) = - SOLI(N2) + SOLI(N3)                              M31.2005
10180 CONTINUE                                                          M31.2006
C.  TRANSFORMATION OF THEV VARIABLES IF UM TYPE 4 :                     M31.2007
10200 IF (JTYPE(JM) .NE. 4) GO TO 10580                                 M38.2121
      DO 10502 N1 = KCLD1,KCLE                                          M31.2009
      IF (NODVO1(N1) .NE. NODVO2(N1)) GO TO 10504                       M31.2010
10502 CONTINUE                                                          M31.2011
      GO TO 10580                                                       M31.2012
10504 N3 = 0                                                            M31.2013
10505 DO 10510 N1 = 1,3                                                 M31.2014
      N2 = N1 + 3                                                       M31.2015
      IF (N3 .EQ. 1) N2 = N1 + 23                                       M31.2016
10510 DUMVEC(N1) = VINP(N2)                                             M31.2017
      CALL MATVEC(PTHETA,DUMVEC)                                        M31.2018
      DO 10520 N1 = 1,3                                                 M31.2019
      N2 = N1 + 3                                                       M31.2020
      IF (N3 .EQ. 1) N2 = N1 + 23                                       M31.2021
10520 VINP(N2) = DUMVEC(N1)                                             M31.2022
      IF (N3 .EQ. 1) GO TO 10525                                        M31.2023
      N3 = 1                                                            M31.2024
      GO TO 10505                                                       M31.2025
10525 DO 10535 N1 = 1,3                                                 M31.2026
      DO 10530 N2 = 1,3                                                 M31.2027
10530 DUMMAT(N1,N2) = PTHETA(N2,N1)                                     M31.2028
10535 CONTINUE                                                          M31.2029
      CALL MATMUL(ZTHS3,DUMMAT)                                         M31.2030
      CALL MATMUL(ZTHS3I,DUMMAT)                                        M31.2031
      N3 = 0                                                            M31.2032
10540 DO 10550 N1 = 1,3                                                 M31.2033
      DO 10545 N2 = 1,3                                                 M31.2034
10545 DUMMAT(N1,N2) = PTHETA(N1,N2)                                     M31.2035
10550 CONTINUE                                                          M31.2036
      IF (N3 .EQ. 1) GO TO 10565                                        M31.2037
      CALL MATMUL(DUMMAT,ZTHS3)                                         M31.2038
      DO 10560 N1 = 1,3                                                 M31.2039
      DO 10555 N2 = 1,3                                                 M31.2040
10555 ZTHS3(N1,N2) = DUMMAT(N1,N2)                                      M31.2041
10560 CONTINUE                                                          M31.2042
      N3 = 1                                                            M31.2043
      GO TO 10540                                                       M31.2044
10565 CALL MATMUL(DUMMAT,ZTHS3I)                                        M31.2045
      DO 10575 N1 = 1,3                                                 M31.2046
      DO 10570 N2 = 1,3                                                 M31.2047
10570 ZTHS3I(N1,N2) = DUMMAT(N1,N2)                                     M31.2048
10575 CONTINUE                                                          M31.2049
C.  START CALCULATIONS OF Y - NORTON :                                  M33. 802
10580 IF (NORTON .NE. 2) GO TO 10588                                    M33. 803
      VINP(1) = CON(3)*SROOT3 + CON(2)*SROOT2 + CON(1)                  M33. 804
      VINP(21) = 0.0                                                    M33. 805
      DO 10586 N1 = KCLD1,KCLE                                          M33. 806
      N10 = N1 - KCL + 1                                                M33. 807
      VINP(N10) = 0.0                                                   M33. 808
10586 VINP(N10+20) = 0.0                                                M33. 809
C.  CALCULATION OF IM MAIN FLUXES :                                     M31.2050
C.       REAL AND IMAGINARY FLUX : FLXD, STORED, FLXQ, STOREQ           M35.1153
10588 JACOB = 1                                                         M33. 810
      FLXD(JM) = 1.0                                                    M31.2053
      STORED = 0.0                                                      M35.1154
      FLXQ(JM) = 0.0                                                    M31.2055
      STOREQ = 0.0                                                      M35.1155
      GO TO 11000                                                       M31.2057
10600 DO 10601 N1 = 1,4                                                 M31.2058
10601 FJM(N1,1) = FJV(N1)                                               M31.2059
      JACOB = 2                                                         M31.2060
      FLXD(JM) = 0.0                                                    M31.2061
      STORED = 1.0                                                      M35.1156
      GO TO 11000                                                       M31.2063
10610 DO 10611 N1 = 1,4                                                 M31.2064
10611 FJM(N1,2) = FJV(N1)                                               M31.2065
      JACOB = 3                                                         M31.2066
      STORED = 0.0                                                      M35.1157
      FLXQ(JM) = 1.0                                                    M31.2068
      GO TO 11000                                                       M31.2069
10620 DO 10621 N1 = 1,4                                                 M31.2070
10621 FJM(N1,3) = FJV(N1)                                               M31.2071
      JACOB = 4                                                         M31.2072
      FLXQ(JM) = 0.0                                                    M31.2073
      STOREQ = 1.0                                                      M35.1158
      GO TO 11000                                                       M31.2075
10630 DO 10631 N1 = 1,4                                                 M31.2076
10631 FJM(N1,4) = FJV(N1)                                               M31.2077
      JACOB = 5                                                         M31.2078
      STOREQ = 0.0                                                      M35.1159
      GO TO 11000                                                       M31.2080
10640 JACOB = 6                                                         M31.2081
      GO TO 10700                                                       M31.2082
10650 FJV(1) = UMCUR(2)                                                 M31.2083
      FJV(2) = UMCUR(22)                                                M31.2084
      FJV(3) = UMCUR(3)                                                 M31.2085
      FJV(4) = UMCUR(23)                                                M31.2086
      IF (KCLD1 .GT. KCLDE) GO TO 10670                                 M31.2087
      DO 10660 N1 = KCLD1,KCLDE                                         M31.2088
      N2 = N1 - KCL + 1                                                 M31.2089
      N3 = N2 + 20                                                      M31.2090
      FJV(1) = FJV(1) + UMCUR(N2)                                       M31.2091
10660 FJV(2) = FJV(2) + UMCUR(N3)                                       M31.2092
10670 IF (KCLQ1 .GT. KCLQE) GO TO 10690                                 M31.2093
      DO 10680 N1 = KCLQ1,KCLQE                                         M31.2094
      N2 = N1 - KCL + 1                                                 M31.2095
      N3 = N2 + 20                                                      M31.2096
      FJV(3) = FJV(3) + UMCUR(N2)                                       M31.2097
10680 FJV(4) = FJV(4) + UMCUR(N3)                                       M31.2098
10690 GO TO (10600,10610,10620,10630,10640), JACOB                      M31.2099
C.  NOW CALCULATE REAL AND IMAGINARY FLUXES BY GAUSSIAN                 M31.2100
C    ELIMINATION AT STMT. NR. 11625 . POINT OF RETURN = 10750           M31.2101
10700 DO 10709 N1 = 1,4                                                 M31.2102
      DO 10708 N2 = 1,4                                                 M31.2103
10708 FJM(N1,N2) = FJM(N1,N2) - FJV(N1)                                 M31.2104
10709 CONTINUE                                                          M31.2105
      DO 10710 N1 = 1,2                                                 M31.2106
      N2 = N1 + 2                                                       M31.2107
      FJM(N1,N1) = FJM(N1,N1) - 1.0/REAMDU(JM)                          M31.2108
10710 FJM(N2,N2) = FJM(N2,N2) - 1.0/REAMQU(JM)                          M31.2109
      DO 10730 N1 = 1,4                                                 M31.2110
      DO 10720 N2 = 1,4                                                 M31.2111
10720 GMAT(N1,N2) = - FJM(N1,N2)                                        M31.2112
10730 CONTINUE                                                          M31.2113
      DO 10740 N1 = 1,4                                                 M31.2114
10740 GMAT(N1,5) = FJV(N1)                                              M31.2115
      JGAUSS = 1                                                        M31.2116
      N5 = 4                                                            M31.2117
      GO TO 11625                                                       M31.2118
10750 JGAUSS = 0                                                        M31.2119
      FLXD(JM) = DUMVEC(1)                                              M31.2120
      STORED = DUMVEC(2)                                                M35.1160
      FLXQ(JM) = DUMVEC(3)                                              M31.2122
      STOREQ = DUMVEC(4)                                                M35.1161
C.   CALCULATION OF UMCUR FOR IM *******************************        M31.2124
C.   UMCUR POWER :                                                      M31.2125
11000 D10 = REACL(KCL+1) * SELTAT                                       M31.2126
      D11 = REACL(KCL+2) * SELTAT                                       M31.2127
      D11 = D11 + ZTHRI + (D10-D11)/2.0                                 M31.2128
      D12 = ZTHRR + 1.0/GPAR(KCL+2)                                     M31.2129
      D13 = 2.0 * (D11*D11 + D12*D12)                                   M31.2130
      D17 = (FLXD(JM) + STOREQ) * SELTAT                                M35.1165
      D18 = (STORED - FLXQ(JM)) * SELTAT                                M35.1166
      D1 = - SROOT2*VINP(1)                                             M31.2131
      D1 = D1 + D17                                                     M37.3590
      D2 = - SROOT2*VINP(21)                                            M31.2133
      D2 = D2 + D18                                                     M37.3591
      UMCUR(1) = 0.0                                                    M31.2135
      UMCUR(21) = 0.0                                                   M35.1169
      UMCUR(2) = (D2*D12 - D1*D11)/D13                                  M37.3592
      UMCUR(3) = (D1*D12 + D2*D11)/D13                                  M37.3593
      UMCUR(22) = - UMCUR(3)                                            M31.2139
      UMCUR(23) = + UMCUR(2)                                            M31.2140
C.  UMCUR EXCITATION BECAUSE OF INPUT VOLTAGE                           M31.2141
      IF (JACOB .LT. 6) GO TO 11100                                     M33. 811
      IF (LOOPSS(9) .NE. 2) GO TO 11100                                 M33. 812
      IF (SLIP .NE. 0.0) GO TO 11100                                    M33. 813
      GO TO 11720                                                       M33. 814
11100 DO 11500 N10 = KCLD1,KCLE                                         M33. 815
      D1 = SLIP * SELTAT * REACL(N10) * GPAR(N10)                       M31.2143
      D2 = 1.0 + D1*D1                                                  M31.2144
      FPAR(N10) = 1.0/D2                                                M31.2145
      N5 = N10 - KCL + 1                                                M31.2146
      N6 = N5 + 20                                                      M31.2147
      D3 = FPAR(N10) * GPAR(N10)                                        M31.2148
      UMCUR(N5) = - D3 * (VINP(N5) + D1*VINP(N6))                       M31.2149
      UMCUR(N6) = - D3 * (VINP(N6) - D1*VINP(N5))                       M31.2150
11500 CONTINUE                                                          M31.2151
C.  UMCUR EXCITATION BECAUSE OF FLUXES                                  M31.2152
11515 IF (NCLD(JM) .EQ. 0) GO TO 11530                                  M31.2153
      DO 11520 N10 = KCLD1,KCLDE                                        M31.2154
      N5 = N10 - KCL + 1                                                M31.2155
      N6 = N5 + 20                                                      M31.2156
      D1 = SLIP * SELTAT * GPAR(N10) * REACL(N10)                       M31.2157
      D2 = SLIP * SELTAT * GPAR(N10) * FPAR(N10)                        M31.2158
      UMCUR(N5) = UMCUR(N5) + D2*(STORED - D1*FLXD(JM))                 M35.1175
      UMCUR(N6) = UMCUR(N6) - D2*(D1*STORED + FLXD(JM))                 M35.1176
11520 CONTINUE                                                          M31.2161
11530 IF (NCLQ(JM) .EQ. 0) GO TO 11550                                  M31.2162
      DO 11540 N10 = KCLQ1,KCLQE                                        M31.2163
      N5 = N10 - KCL + 1                                                M31.2164
      N6 = N5 + 20                                                      M31.2165
      D1 = SLIP * SELTAT * GPAR(N10) * REACL(N10)                       M31.2166
      D2 = SLIP * SELTAT * GPAR(N10) * FPAR(N10)                        M31.2167
      UMCUR(N5) = UMCUR(N5) + D2*(STOREQ - D1*FLXQ(JM))                 M35.1177
      UMCUR(N6) = UMCUR(N6) - D2*(D1*STOREQ + FLXQ(JM))                 M35.1178
11540 CONTINUE                                                          M31.2170
C.  UMCUR EXCITATION BECAUSE OF THEV IMPEDANCES :                       M31.2171
11550 DO 11552 N1 = KCLD1,KCLE                                          M31.2172
      IF (NODVO1(N1) .NE. NODVO2(N1)) GO TO 11554                       M31.2173
11552 CONTINUE                                                          M31.2174
      GO TO 11720                                                       M31.2175
11554 N15 = KCLE                                                        M31.2176
      IF (NCL .GT. 6) N15 = KCL + 5                                     M31.2177
      DO 11555 N10 = 1,3                                                M31.2178
      DO 11555  N11 = 1, 3                                              M31.2179
      ZTHEVR(N10,N11) = 0.0                                             M31.2180
11555 DUMMAT(N10,N11) = 0.0                                             M31.2181
      N7 = KCL + 3                                                      M31.2182
      DO 11560 N10 = N7,N15                                             M31.2183
      N6 = N10 - KCL - 2                                                M31.2184
      ZTHEVR(N6,N6) = GPAR(N10) * FPAR(N10)                             M31.2185
11560 DUMMAT(N6,N6) = ZTHEVR(N6,N6)                                     M31.2186
      CALL MATMUL(DUMMAT,ZTHS3)                                         M31.2187
      CALL MATMUL(ZTHEVR,ZTHS3I)                                        M31.2188
      DO 11565 N10 = 1,3                                                M31.2189
      DO 11565 N11 = 1,3                                                M31.2190
      DUMMA1(N10,N11) = DUMMAT(N10,N11)                                 M31.2191
11565 DUMMA2(N10,N11) = ZTHEVR(N10,N11)                                 M31.2192
      DO 11580 N10 = 1,3                                                M31.2193
      N12 = N10 + KCL + 2                                               M31.2194
      D1 = SLIP * SELTAT * REACL(N12) * GPAR(N12)                       M31.2195
      DO 11570 N11 = 1,3                                                M31.2196
      ZTHEVR(N10,N11) = D1 * ZTHEVR(N10,N11)                            M31.2197
11570 DUMMAT(N10,N11) = D1 * DUMMAT(N10,N11)                            M31.2198
11580 CONTINUE                                                          M31.2199
      DO 11590 N10 = 1,3                                                M31.2200
      DO 11590 N11 = 1,3                                                M31.2201
      DUMMA1(N10,N11) = DUMMA1(N10,N11) + ZTHEVR(N10,N11)               M31.2202
11590 DUMMA2(N10,N11) = DUMMA2(N10,N11) - DUMMAT(N10,N11)               M31.2203
      DO 11605 N10 = 1,3                                                M31.2204
      N12 = N10 + 3                                                     M31.2205
      DO 11600 N11 = 1,3                                                M31.2206
      N13 = N11 + 3                                                     M31.2207
      GMAT(N10,N11) = + DUMMA1(N10,N11)                                 M31.2208
      GMAT(N12,N13) = + DUMMA1(N10,N11)                                 M31.2209
      GMAT(N10,N13) = - DUMMA2(N10,N11)                                 M31.2210
11600 GMAT(N12,N11) = + DUMMA2(N10,N11)                                 M31.2211
11605 CONTINUE                                                          M31.2212
      DO 11610 N10 = 1,6                                                M31.2213
11610 GMAT(N10,N10) = GMAT(N10,N10) + 1.0                               M31.2214
      DO 11620 N10 = 1,3                                                M31.2215
      N11 = N10 + 3                                                     M31.2216
      N12 = N11 + 20                                                    M31.2217
      GMAT(N10,7) = UMCUR(N11)                                          M31.2218
11620 GMAT(N11,7) = UMCUR(N12)                                          M31.2219
      JGAUSS = 0                                                        M31.2220
      N5 = 6                                                            M31.2221
C.  GAUSSIAN ELIMINATION FOR INPUT : N5 AND GMAT(N5,N5+1) *****         M31.2222
C             OUTPUT IS STORED IN  : DUMVEC(N5)                         M31.2223
11625 N6 = N5 + 1                                                       M31.2224
      DO 11640 N1 = 1,N5                                                M31.2225
      N2 = N1 + 1                                                       M31.2226
      IF (N2 .GT. N5) GO TO 11650                                       M31.2227
      DO 11640 N10 = N2,N5                                              M31.2228
      N8 = N6 - N1                                                      M31.2229
      DO 11630 N9 = 1,N8                                                M31.2230
      N11 = N6 + 1 - N9                                                 M31.2231
      D1 = GMAT(N10,N1) * GMAT(N1,N11)/GMAT(N1,N1)                      M31.2232
11630 GMAT(N10,N11) = GMAT(N10,N11) - D1                                M31.2233
      N11 = N6 - N8                                                     M31.2234
      GMAT(N10,N11) = 0.0                                               M31.2235
11640 CONTINUE                                                          M31.2236
11650 DO 11670 N10 = 1,N5                                               M31.2237
      N7 = N5 + 1 - N10                                                 M31.2238
      DUMVEC(N7) = GMAT(N7,N6)/GMAT(N7,N7)                              M31.2239
      DO 11660 N12 = 1,N5                                               M31.2240
      N13 = N7 + N12                                                    M31.2241
      IF (N13 .GT. N5) GO TO 11670                                      M31.2242
11660 DUMVEC(N7)=DUMVEC(N7)-GMAT(N7,N13)*DUMVEC(N13)/GMAT(N7,N7)        M31.2243
11670 CONTINUE                                                          M31.2244
C.  END OF GAUSSIAN ELIMINATION ********************************        M31.2245
      IF (JGAUSS .EQ. 1) GO TO 10750                                    M31.2246
      DO 11680 N10 = 1,3                                                M31.2247
      N11 = N10 + 3                                                     M31.2248
      N12 = N11 + 20                                                    M31.2249
      UMCUR(N11) = DUMVEC(N10)                                          M31.2250
11680 UMCUR(N12) = DUMVEC(N11)                                          M31.2251
11700 IF (NCL .LE. 6) GO TO 11720                                       M31.2252
      N15 = KCL + 6                                                     M31.2253
      DO 11710 N10 = N15,KCLE                                           M31.2254
      N11 = N10 - KCL + 1                                               M31.2255
      N12 = N11 + 20                                                    M31.2256
      ZTHEVS(N11) = GPAR(N10) * FPAR(N10) * ZTHEVS(N11)                 M31.2257
      ZTHEVS(N12) = GPAR(N10) * FPAR(N10) * ZTHEVS(N12)                 M31.2258
      D1 = SLIP * SELTAT * GPAR(N10) * REACL(N10)                       M31.2259
      D2 = ZTHEVS(N11)                                                  M31.2260
      ZTHEVS(N11) = 1.0 + ZTHEVS(N11) + D1*ZTHEVS(N12)                  M31.2261
      ZTHEVS(N12) = + ZTHEVS(N12) - D1*D2                               M31.2262
      D3 = ZTHEVS(N11) * ZTHEVS(N11)                                    M31.2263
      D3 = D3 + ZTHEVS(N12)*ZTHEVS(N12)                                 M31.2264
      D3 = 1.0/D3                                                       M31.2265
      D1 = UMCUR(N11)                                                   M31.2266
      UMCUR(N11) = ZTHEVS(N11)*UMCUR(N11)+ZTHEVS(N12)*UMCUR(N12)        M31.2267
      UMCUR(N11) = D3 * UMCUR(N11)                                      M31.2268
      UMCUR(N12) = - ZTHEVS(N12)*D1 + ZTHEVS(N11)*UMCUR(N12)            M31.2269
11710 UMCUR(N12) = D3 * UMCUR(N12)                                      M31.2270
11720 IF (JACOB .LT. 6) GO TO 10650                                     M31.2271
C.  OUTPUT FOR UM = IM                                                  M31.2272
      IF (LOOPSS(9) .EQ. 2) GO TO 11800                                 M37.3594
11725 OMEGM(JM) = (1.0 - SLIP)*SELTAT/NPPAIR(JM)                        M37.3595
      OMOLD(JM) = OMEGM(JM)                                             M31.2275
      DO 11730 N10 = 1,3                                                M31.2276
      N11 = KCL - 1 + N10                                               M31.2277
11730 HIST(N11) = UMCUR(N10)                                            M31.2278
      DO 11740 N10 = KCLD1,KCLE                                         M31.2279
      N11 = N10 - KCL + 1                                               M31.2280
11740 HIST(N10) = - UMCUR(N11)                                          M31.2281
C.  BACK TRANSFORMATION FOR UM TYPE 4 EXCITATION CURRENTS               M31.2282
11750 IF (JTYPE(JM) .NE. 4) GO TO 11800                                 M31.2283
      N13 = 0                                                           M31.2284
11760 DO 11780 N10 = 1,3                                                M31.2285
      N12 = N10 + 3 + N13*20                                            M31.2286
      DUMVEC(N10) = UMCUR(N12)                                          M31.2287
      DO 11770 N11 = 1,3                                                M31.2288
11770 DUMMAT(N10,N11) = PTHETA(N11,N10)                                 M31.2289
11780 CONTINUE                                                          M31.2290
      CALL MATVEC(DUMMAT,DUMVEC)                                        M31.2291
      DO 11790 N10 = 1,3                                                M31.2292
      N11 = N10 + 3 + N13*20                                            M31.2293
11790 UMCUR(N11) = DUMVEC(N10)                                          M31.2294
      IF(N13 .EQ. 1) GO TO 11800                                        M31.2295
      N13 = 1                                                           M31.2296
      GO TO 11760                                                       M31.2297
C.  OUTPUT INTERFACE OF IM AND ELECT. NETWORK :                         M31.2298
11800 D14 = CON(3)/(SROOT2*SROOT3) + CON(2)/2.0 + CON(1)                M33. 818
      D15 = CON(3)/SROOT3 + CON(2)/SROOT2 + CON(1)                      M33. 819
      D11 = (- UMCUR(22) + UMCUR(3)) * D14                              M38.2122
      D12 = (+ UMCUR(2) + UMCUR(23)) * D14                              M38.2123
C  ADJUSTING CURRENT OUTPUT TO INCLUDE POWER RESISTANCE TO EMTP         M37.3599
C   FOR THE CASE OF PREDICTION REQUEST :                                M37.3600
      IF (LOOPSS(9) .NE. 3) GO TO 11806                                         
      D11 = D11 + VINP(1)*GPAR(KCL+2)*D15                               M38.2124
      D12 = D12 + VINP(21)*GPAR(KCL+2)*D15                              M37.3610
11806 D10 = D11*D11 + D12*D12                                           M37.3611
      IF (D10 .NE. 0.0) GO TO 11810                                     M33. 824
      D11 = 0.0                                                         M33. 825
      D2 = 0.0                                                          M33. 826
      D3 = 0.0                                                          M33. 827
      GO TO 11815                                                       M33. 828
11810 IF (NORTON .EQ. 2) D5 = - D11/D10                                 M33. 829
      IF (NORTON .EQ. 2) D6 = + D12/D10                                 M33. 830
      D10 = SQRTZ(D10)                                                  M31.2302
      D1 = D12/D11                                                      M31.2304
      D1 = ATANZ(D1)                                                    M31.2305
      IF (D11 .LT. 0.0) D1 = D1 + TWOPI/2.0                             M31.2306
      D2 = CON(2)*D1 + CON(3)*(D1 - TWOPI/3.0)                          M31.2307
      D3 = CON(2)*(D1 + TWOPI/4.0) + CON(3)*(D1 + TWOPI/3.0)            M31.2308
      D3 = D3 + CON(1)*D1                                               M31.2309
11815 DO 11824 N1 = 1,3                                                 M33. 831
      N2 = N1 + KCL - 1                                                 M31.2311
      N11 = 3 * (JM-1)                                                  M33. 832
      IF (NODVO1(N2) .EQ. NODVO2(N2)) GO TO 11824                       M33. 833
C      INCLUDING MACH RESISTANCES TO POWER NETWORK AT                   M33. 834
C      LAST PASS THROUGH THIS PART OF CODE FOR NON -                    M33. 835
C      COMPENSATED POWER COILS.                                         M33. 836
      IF (LFIM3I .NE. 0) GO TO 11818                                    M38.2125
      IF (LOOPSS(8) .NE. 1) GO TO 11821                                 M33. 837
11818 N11 = NUMIBR - 3 + N1                                             M38.2126
      N12 = - NR(N11)                                                   M37.3613
      IF (N12 .LT. 0) N12 = - N12                                       M37.3614
      TR(N12) = 1.0/GPAR(KCL+2)                                         M33. 839
      TX(N12) = 0.0                                                     M33. 840
      IF (NORTON .NE. 2) GO TO 11821                                    M33. 841
      TR(N12) = D5                                                      M33. 842
      TX(N12) = D6 * 1.0D+3/SELTAT                                              
      IF (XOPT .NE. 0.0) TX(N12) = D6 * XOPT * TWOPI/SELTAT             M39.1398
      GO TO 11823                                                       M36.1001
11821 D11 = D1                                                          M33. 846
      IF (N1 .EQ. 2) D11 = D2                                           M31.2314
      IF (N1 .EQ. 3) D11 = D3                                           M31.2315
      IF (N1 .EQ. 1 .AND. LFIM3I .NE. 0) NCLOUT = HIST(KCL+2)           M38.2127
      IF (NODVO1(N2) .EQ. 1) GO TO 11822                                M33. 847
      CREST(NCLOUT) = D10                                               M31.2317
      TIME1(NCLOUT) = D11                                               M31.2318
      IF (IPRSUP .GE. 1)                                                M36.1002
     1 WRITE(LUNIT6,8862) NCLOUT,SFREQ(NCLOUT),CREST(NCLOUT),           M36.1003
     2                    TIME1(NCLOUT),TSTOP(NCLOUT)                   M36.1004
      NCLOUT = NCLOUT + 1                                               M37.3615
11822 IF (NODVO2(N2) .EQ. 1) GO TO 11823                                M36.1005
      CREST(NCLOUT) = - D10                                             M31.2321
      TIME1(NCLOUT) = D11                                               M31.2322
      IF (IPRSUP .GE. 1)                                                M36.1006
     1 WRITE(LUNIT6,8862) NCLOUT,SFREQ(NCLOUT),CREST(NCLOUT),           M36.1007
     2                    TIME1(NCLOUT),TSTOP(NCLOUT)                   M36.1008
      NCLOUT = NCLOUT + 1                                               M31.2323
11823 IF (LOOPSS(8) .EQ. 0 .AND. LFIM3I .EQ. 0) GO TO 11824             M38.2128
      IF (IPRSUP .GE. 1 .AND. NORTON .NE. 1)                            M37.3617
     1 WRITE(LUNIT6,8860) N11,TR(N12),TX(N12)                           M37.3618
11824 CONTINUE                                                          M33. 849
      IF (LOOPSS(9) .NE. 2) GO TO 11825                                 M33. 850
      IF (NORTON .EQ. 2) GO TO 11825                                    M33. 851
      NORTON = 2                                                        M33. 852
      GO TO 10580                                                       M33. 853
11825 IF (LFIM3I .NE. 0) GO TO 13050                                    M38.2129
      NCLIND = FPAR(KCL+1)                                              M38.2130
      DO 11840 N10 = 4,NCL                                              M37.3621
      N11 = N10 + 20                                                    M31.2326
      N12 = N10 + KCL - 1                                               M31.2327
      IF (SLIP .LT. 0.0) UMCUR(N11) = - UMCUR(N11)                      M35.1186
      IF (NODVO1(N12) .EQ. NODVO2(N12)) GO TO 11840                     M31.2328
      IF (LOOPSS(9) .EQ. 2) GO TO 11828                                 M37.3622
      D2 = UMCUR(N10)*UMCUR(N10) + UMCUR(N11)*UMCUR(N11)                M31.2329
      D2 = SQRTZ(D2)                                                    M31.2330
      D3 = UMCUR(N11)/UMCUR(N10)                                        M31.2331
      D3 = ATANZ(D3)                                                    M31.2332
      IF (UMCUR(N10) .LT. 0.0) D3 = D3 + TWOPI/2.0                      M31.2333
11828 IF (NODVO1(N12) .EQ. 1) GO TO 11830                               M35.1195
      IF (LOOPSS(9) .EQ. 2) GO TO 11829                                 M37.3623
      CREST(NCLIND) = - D2                                              M37.3624
      TIME1(NCLIND) = D3                                                M37.3625
      IF (IPRSUP .GE. 1)                                                M36.1011
     1 WRITE(LUNIT6,8862) NCLIND,SFREQ(NCLIND),CREST(NCLIND),           M37.3626
     2                    TIME1(NCLIND),TSTOP(NCLIND)                   M37.3627
11829 NCLIND = NCLIND + 1                                               M37.3628
11830 IF (NODVO2(N12) .EQ. 1) GO TO 11840                               M33. 859
      IF (LOOPSS(9) .EQ. 2) GO TO 11832                                 M37.3629
      CREST(NCLIND) = + D2                                              M37.3630
      TIME1(NCLIND) = D3                                                M37.3631
      IF (IPRSUP .GE. 1)                                                M36.1014
     1 WRITE(LUNIT6,8862) NCLIND,SFREQ(NCLIND),CREST(NCLIND),           M37.3632
     2                    TIME1(NCLIND),TSTOP(NCLIND)                   M37.3633
11832 NCLIND = NCLIND + 1                                               M37.3634
11840 CONTINUE                                                          M31.2342
C.  CALCULATION OF MECH. VARIABLES *****************************        M31.2343
C   NOTE : THIS SECOND PART OF UMRENU IS ENTERED ONLY AT THE            M37.3665
C          LAST PASS THROUGH SECOND PART OF CODE. HERE WE               M37.3666
C          WILL USE ANGLUM(JM) TO STORE TQGEN FOR THE SLACK-BUS         M37.3667
C          CALCULATION.                                                 M37.3668
11900 IF (LOOPSS(9) .EQ. 2) GO TO 12100                                 M39.1399
12000 D1 = 3.0*CON(3) + 2.0*CON(2) + CON(8) + CON(1)                    M31.2344
      D2 = (REACL(KCL+1) - REACL(KCL+2))*UMCUR(2)*UMCUR(3)              M31.2345
      D3 = UMCUR(2) * FLXQ(JM)                                          M31.2346
      D4 = UMCUR(3) * FLXD(JM)                                          M31.2347
      TQGEN = (D2 - D3 + D4) * NPPAIR(JM)                               M31.2348
      IF (INPU .EQ. 1) TQGEN = TQGEN/D1                                 M31.2349
C.  OUTPUT INTERFACE OF UM WITH MECHANICAL NETWORK :                    M31.2350
      ANGLUM(JM) = + TQGEN                                              M37.3670
      CREST(NCLOUT) = OMEGM(JM)                                         M39.1400
      IF (LOOPSS(10) .NE. 3) JTMTAC(JM) = 0                             M39.1401
      IF (IPRSUP .GE. 1)                                                M39.1402
     1 WRITE(LUNIT6,8862) NCLOUT,SFREQ(NCLOUT),CREST(NCLOUT),           M39.1403
     2                    TIME1(NCLOUT),TSTOP(NCLOUT)                   M39.1404
      IF (IPRSUP .GE. 1)                                                M37.3671
     1 WRITE(LUNIT6,12010) (HIST(N1),N1 = 1,NCL)                        M37.3672
12010 FORMAT(/,14H HIST(1:NCL) =, (1X,6E15.6))                          M37.3673
      IF (IPRSUP .GE. 1) WRITE(LUNIT6,12020) TQGEN                      M37.3674
12020 FORMAT(/, 8H TQGEN =, 1X,E15.6)                                   M37.3675
12100 NCLOUT = NCLOUT + 1                                               M39.1405
C  FINAL STATEMENTS OF MACHINE DO-LOOP                                  M32.3252
13000 IF (NTYP59 .EQ. 1) JTYPE(JM) = 13                                 M32.3253
      IF (NTYP59 .EQ. 1) INPU = INPUST                                  M32.3254
      IF (LOOPSS(9) .EQ. 2) HIST(KCL) = NUMIBR                          M37.3677
13050 CONTINUE                                                                  
      IF (LFIM3 .EQ. 1) LOOPSS(9) = LPSS9                               M38.2143
      IF (LFIM3 .EQ. 1) GO TO 13500                                             
      IF (LOOPSS(9) .EQ. 2) GO TO 13500                                         
13060 LOOPSS(2) = 0                                                     M33. 868
      LOOPSS(6) = KCONST                                                        
C.  CALCULATION OF SLACK BUSES :                                        M31.2356
13100 KCONST = LOOPSS(6)                                                M38.2146
      DO 13370 JM = 1,NUMUM                                             M38.2147
      KCL = KCOIL(JM)                                                   M38.2148
      N1 = JCLTAC(KCL)                                                          
      N2 = JCLTAC(KCL+1)                                                        
      NSHARE = N1 + N2                                                  M39.1408
      IF (NSHARE .NE. 0) NSHARE = 1                                     M39.1409
C  NOTE : IF NSHARE=0 THEN NO MULTI UM SHARING MECH NETW                M39.1410
      IF (N1 .NE. 0 .AND. JM .GT. N1) NSHARE = 10                       M39.1411
      IF (N2 .NE. 0 .AND. JM .GT. N2) NSHARE = 10                       M39.1412
C  NOTE: IF NSHARE=10, NO MECH SLACK BUS CALC BUT SPEED CAPACITOR       M39.1413
C         AND TORQUE SOURCE OF UM NR. N1 AND UM NR. N2 TO BE SET        M39.1414
      IF (IPRSUP .GE. 1) WRITE (LUNIT6,13110) JM                        M36.1020
13110 FORMAT(/,26H *************************,                           M36.1021
     1         39H START ADJUSTMENT OF SLACK BUSES IN THE,              M36.1022
     2         37H EMTP NETWORKS CONNECTED TO UM NUMBER,I4,2H :)        M36.1023
C  CHECKING SPEEDS FOR MULTI-MACH SHARING COMMON NETWORK :              M39.1415
      IF (LOOPSS(4) .GT. 0) GO TO 13118                                 M39.1416
      IF (NSHARE .EQ. 0) GO TO 13118                                    M39.1417
      IF (NSHARE .EQ. 10) GO TO 13118                                   M39.1418
      IF (OMEGM(JM) .NE. OMEGM(N1)) GO TO 13112                         M39.1419
      IF (N2 .EQ. 0) GO TO 13118                                        M39.1420
      IF (OMEGM(JM) .NE. OMEGM(N2)) GO TO 13112                         M39.1421
      GO TO 13118                                                       M39.1422
13112 IF (N2 .EQ. 0) GO TO 13115                                        M39.1423
      WRITE(LUNIT6,13113) JM,N1,N2                                      M39.1424
13113 FORMAT(/,45H ********************************************,        M39.1425
     1         45H ********************************************,        M39.1426
     2         20H WARNING : UM NUMBER,I4,2H, ,I4,5H, AND,I4)           M39.1427
      WRITE (LUNIT6,13114)                                              M39.1428
13114 FORMAT(  42H SHARE A COMMON MECHANICAL NETWORK, BUT DO,           M39.1429
     1         45H NOT HAVE EQUAL MECHANICAL MACHINE-SPEEDS. IS,        M39.1430
     2         43H THIS INTENTIONAL OR HAVE YOU MADE AN ERROR           M39.1431
     3   ,/,   45H IN DESIGNING YOUR MECHANICAL NETWORK ? THESE,        M39.1432
     4         45H SPEEDS (RAD/SEC) ARE EQUAL TO (1-SLIP) TIMES,        M39.1433
     5         44H ANGULAR FREQ OF ELECTRIC POWER GRID DEVIDED)         M39.1434
      WRITE(LUNIT6,93116)                                               M39.1435
93116 FORMAT(  39H BY THE NR OF POLE-PAIRS. THE SLIP HERE,              M39.1436
     1         42H IS IN ABS. VALUES AND IN STEADY-STATE CAN,           M39.1437
     2         40H OBVIOUSLY ONLY BE NONZERO FOR INDUCTION,             M39.1438
     3         10H MACHINES.,/)                                         M39.1439
13115 WRITE(LUNIT6,13116) JM,N1                                         M39.1440
13116 FORMAT(/,45H ********************************************,        M39.1441
     1         45H ********************************************,        M39.1442
     2         20H WARNING : UM NUMBER,I4,5H, AND,I4)                   M39.1443
      WRITE(LUNIT6,13114)                                               M39.1444
      WRITE(LUNIT6,93116)                                               M39.1445
13118 IF (IPRSUP .GE. 1) WRITE(LUNIT6,13120)                            M39.1446
13120 FORMAT(30H *********** SLACK BUS SOURCE:,2X,6HKCONST,             M36.1025
     1        4X,4HNODE,9X,5HSFREQ,9X,5HCREST,9X,5HTIME1,               M36.1026
     2        8X,6HTSTART,9X,5HTSTOP)                                   M36.1027
      IMACH = 1                                                         M31.2358
      IF (JTYPE(JM) .LT. 3) IMACH = 0                                   M31.2359
      IF (JTYPE(JM) .GT. 7) IMACH = 0                                   M31.2360
      IF (IMACH .NE. 1) N10 = NODFUM(JM)                                M31.2361
      IF (LOOPSS(4) .GT. 0) GO TO 13130                                 M38.2149
      IF (IMACH .NE. 1) CREST(N10) = VOLTUM(JM)                         M32.3257
      IF (IMACH .NE. 1 .AND. IPRSUP .GE. 1)                             M36.1028
     1 WRITE(LUNIT6,13122) N10,NODE(N10),SFREQ(N10),CREST(N10),         M36.1029
     2                     TIME1(N10),TSTART(N10),TSTOP(N10)            M36.1030
13122 FORMAT(30H ****** EXCT SLACK BUS SOURCE:,2X,I6,2X,I6,5E14.5)      M37.3679
13123 FORMAT(30H ****** MECH SLACK BUS SOURCE:,2X,I6,2X,I6,5E14.5)      M37.3680
      IF (NSHARE .EQ. 10) GO TO 13170                                   M39.1447
      GO TO 13370                                                       M38.2150
C  COIL TERMINAL VOLTAGE AND MECHANICAL SPEED FOR GIVEN CREST:          M31.2367
C   TEMPORARY STORAGE OF VOLTAGE IN FLXD(JM) AND VOLTUM(JM)             M31.2368
C   TEMPORARY STORAGE FOR TORQUE IN FLXQ(JM) AND ANGLUM(JM)             M37.3681
C  NOTE: SOLR IS HERE NODE VOLTAGE SINCE NO COMPENSATION ON             M31.2370
C        THE UM TERMINALS WHICH ARE CONSIDERED HERE.                    M31.2371
C  CODE FOR LOOPSS(4) = 1 AND 2 ********************************        M38.2151
13130 N11 = NODMUM(JM)                                                  M38.2152
      IF (IMACH .EQ. 1) GO TO 13140                                     M38.2155
      N5 = KCL + 3                                                      M38.2156
      N1 = NODVO1(N5)                                                   M31.2374
      N1 = NORDER(N1)                                                   M31.2375
      N2 = NODVO2(N5)                                                   M31.2376
      N2 = NORDER(N2)                                                   M31.2377
      D1 = SOLR(N1) - SOLR(N2)                                          M31.2378
13140 N12 = NODOM(JM)                                                   M38.2157
      N3 = NORDER(N12)                                                  M38.2158
      D3 = - E(N3)                                                      M31.2381
C  CODE FOR LOOPSS(4) = 1  *************************************        M38.2161
      IF (LOOPSS(4) .GT. 1) GO TO 13150                                 M38.2162
      IF (IMACH .NE. 1) FLXD(JM) = D1                                   M31.2383
      IF (IMACH .NE. 1) CREST(N10) = 0.0                                M32.3259
      IF (IMACH .NE. 1 .AND. IPRSUP .GE. 1)                             M36.1032
     1 WRITE(LUNIT6,13122) N10,NODE(N10),SFREQ(N10),CREST(N10),         M36.1033
     2                     TIME1(N10),TSTART(N10),TSTOP(N10)            M36.1034
      IF (NSHARE .EQ. 10) GO TO 13170                                           
13141 FLXQ(JM) = D3                                                     M38.2166
      TSTOP(N11) = CREST(N11)                                           M32.3260
      CREST(N11) = 0.0                                                  M32.3261
      IF (IPRSUP .GE. 1)                                                M36.1035
     1 WRITE(LUNIT6,13123) N11,NODE(N11),SFREQ(N11),CREST(N11),         M37.3682
     2                     TIME1(N11),TSTART(N11),TSTOP(N11)            M36.1037
      N13 = KSSFRQ(N12)                                                 M38.2167
      IF (N13 .LT. 0) GO TO 13370                                       M38.2168
      N14 = N13                                                         M32.3264
13142 IF (N13 .EQ. N11) GO TO 13144                                     M32.3265
      IF (TSTART(N13) .NE. -7777.) GO TO 13144                          M32.3266
      TSTOP(N13) = CREST(N13)                                           M32.3267
      CREST(N13) = 0.0                                                  M32.3268
      IF (IPRSUP .GE. 1)                                                M36.1038
     1 WRITE(LUNIT6,13123) N13,NODE(N13),SFREQ(N13),CREST(N13),         M37.3683
     2                     TIME1(N13),TSTART(N13),TSTOP(N13)            M36.1040
13144 N13 = KPSOUR(N13)                                                 M32.3269
      IF (N14 .NE. N13) GO TO 13142                                     M32.3270
13148 GO TO 13370                                                       M38.2169
C  CODE FOR LOOPSS(4) = 2 (FINAL UMRENU PASS) ******************        M38.2170
13150 IF (IMACH .EQ. 1) GO TO 13160                                     M31.2389
      D5 = VOLTUM(JM) - D1                                              M32.3274
      D5 = D5/(FLXD(JM) - D1)                                           M32.3275
      CREST(N10) = D5 * VOLTUM(JM)                                      M32.3276
      IF (IPRSUP .GE. 1)                                                M36.1041
     1 WRITE(LUNIT6,13122) N10,NODE(N10),SFREQ(N10),CREST(N10),         M36.1042
     2                     TIME1(N10),TSTART(N10),TSTOP(N10)            M36.1043
13160 IF (NSHARE .EQ. 10) GO TO 13170                                   M39.1449
13161 D5 = ANGLUM(JM) - D3                                              M38.2174
      D5 = D5/(FLXQ(JM) - D3)                                           M32.3278
      CREST(N11) = D5 * TSTOP(N11)                                      M32.3279
      TSTOP(N11) = 9999.                                                M32.3280
      IF (IPRSUP .GE. 1)                                                M36.1044
     1 WRITE(LUNIT6,13123) N11,NODE(N11),SFREQ(N11),CREST(N11),         M37.3685
     2                     TIME1(N11),TSTART(N11),TSTOP(N11)            M36.1046
      N13 = KSSFRQ(N12)                                                 M32.3281
      IF (N13 .LT. 0) GO TO 13370                                       M38.2175
      N14 = N13                                                         M32.3283
13162 IF (N13 .EQ. N11) GO TO 13164                                     M32.3284
      IF (TSTART(N13) .NE. -7777.) GO TO 13164                          M32.3285
      CREST(N13) = D5 * TSTOP(N13)                                      M32.3286
      TSTOP(N13) = 9999.                                                M32.3287
      IF (IPRSUP .GE. 1)                                                M36.1047
     1 WRITE(LUNIT6,13123) N13,NODE(N13),SFREQ(N13),CREST(N13),         M37.3686
     2                     TIME1(N13),TSTART(N13),TSTOP(N13)            M36.1049
13164 N13 = KPSOUR(N13)                                                 M32.3288
      IF (N14 .NE. N13) GO TO 13162                                     M32.3289
C  SET SPEED CAPACITORS AND SOURCES FOR TQGEN (FOR UM'S WITH            M38.2177
C    NO SLACK BUS CALCULATIONS, I.E. JM GT THAN JCLTAC(KCL) AND         M38.2178
C    FOR JM GT JCLTAC(KCL+1) .                                          M38.2179
13170 IF (NSHARE .NE. 10) GO TO 13370                                   M39.1450
      N2 = FPAR(KCL+2)                                                          
      C(N2) = 1.0D+8                                                    M39.1452
      IF (COPT .NE. 0.0) C(ITCAP) = C(ITCAP)*COPT*TWOPI                 M38.2181
      TR(N2) = 0.0                                                      M39.1453
      KCONST = KCONST + 1                                               M38.2182
      IF ( KCONST .LE. LEXCT )  GO TO 6784                              M42.4977
      WRITE (LUNIT6, 6783)  LEXCT                                       M42.4978
 6783 FORMAT ( 38H OVERFLOW LIST 4 IN "UMRENU".  LEXCT =,               M42.4979
     1  I5,    18H     STOP LOCALLY.     )                              M42.4980
      CALL STOPTP                                                       M42.4981
 6784 NODE(KCONST) = - NODOM(JM)                                        M42.4982
      IFORM(KCONST) = 14                                                M38.2184
      CREST(KCONST) = - ANGLUM(JM)                                      M38.2185
      TIME1(KCONST) = 0.0                                               M38.2186
      TSTART(KCONST) = - 1.0                                            M38.2187
      TSTOP(KCONST) = 0.0                                               M38.2188
      N5 = NODMUM(JM)                                                   M38.2189
      SFREQ(KCONST) = SFREQ(N5)                                         M38.2190
      IF (IPRSUP .GE. 1)                                                M38.2191
     1 WRITE(LUNIT6,13172) KCONST,NODE(KCONST),SFREQ(KCONST),           M38.2192
     2                     CREST(KCONST),TIME1(KCONST),                 M38.2193
     3                     TSTART(KCONST),TSTOP(KCONST)                 M38.2194
13172 FORMAT(31H ****** TEMPORARY TQGEN SOURCE:,1X,I6,2X,               M38.2195
     1         I6,5E14.5)                                               M38.2196
13370 CONTINUE                                                          M38.2197
      LOOPSS(4) = LOOPSS(4) + 1                                         M31.2397
      IF (LOOPSS(4) .LT. 3) GO TO 13500                                 M38.2198
C.  PERMANENT EXIT OF UMRENU IF LOOPSS(4) .GE. 3                        M38.2199
      LOOPSS(1) = - 1                                                           
      KSSOUT = LOOPSS(7)                                                M31.2401
      ISTART = 0                                                        M31.2402
      IF (IPRSUP .GE. 1) WRITE (LUNIT6,13400)                           M31.2403
13400 FORMAT( /, 40H PERMANENT EXIT OF UMRENU **************)           M31.2404
      GO TO 14000                                                       M31.2405
13500 ISTART = ISTART + 1                                               M31.2406
      IF (IPRSUP .LT. 4) GO TO 14000                                    M36.1051
      WRITE (LUNIT6, 13616)  (I, KBUS(I), MBUS(I), NR(I), LENGTH(I),    M31.2408
     1   KODEBR(I), KODSEM(I), I = 1, IBR)                              M31.2409
13616 FORMAT (/, 21H BRANCH TABLE VECTORS,/,  70H       ROW      KBUS   M31.2410
     1   MBUS        NR    LENGTH    KODEBR    KODSEM,/, (1X, 7I10) )   M31.2411
      WRITE (LUNIT6, 13716)  (I, IFORM(I), NODE(I), CREST(I), TIME1(I), M31.2412
     1   TSTART(I), SFREQ(I), I = 1, KCONST)                            M31.2413
13716 FORMAT (/, 21H SOURCE TABLE VECTORS,/, 30H       ROW     IFORM    M31.2414
     1  NODE,15X, 5HCREST, 15X, 5HTIME1, 14X, 6HTSTART,15X,5HSFREQ,/,   M31.2415
     2   (3I10, 4E20.10)   )                                            M31.2416
14000 IF (IPRSUP .GE. 1)                                                M36.1052
     1 WRITE(LUNIT6, 14100)  ISTART,ISTEP,LOOPSS(1),LOOPSS(2),          M38.2201
     2   LOOPSS(4),LOOPSS(8),LOOPSS(9),LOOPSS(10),KCONST,IBR,           M38.2202
     3   INONL,KSWTCH,ISTEAD,OMEGRF                                     M38.2203
14100 FORMAT (/,26H *************************,                          M37.3688
     1          30H SETTING AT THIS UMRENU EXIT :                       M37.3689
     2  ,/, 48H  ISTART   ISTEP  LOPSS1  LOPSS2  LOPSS4  LOPSS8,        M38.2204
     3      48H  LOPSS9 LOPSS10  KCONST     IBR   INONL  KSWTCH,        M38.2205
     4       8H  ISTEAD,8X,6HOMEGRF,/,13I8,E14.5, /)                    M38.2206
      RETURN                                                            M36.1061
      END                                                               M31.2424
      SUBROUTINE UMRNU2( REACL,GPAR,FPAR, HIST,UMCURP,NODVO1,           M42.4983
     2 JTYPE,NODOM, JTMTAC,THETAM, IMACH,REAMDU, REAMQU,NODVO2,         M42.4984
     3 NPPAIR,ROTMOM, NCLD,JCLTAC, KCL,NCLQ, EPSOM,DCOEF,               M42.4985
     4 KCOIL,VOLTUM, ANGLUM,NODFUM, NODMUM, UMOUTP )                    M42.4986
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION  REACL(1), GPAR(1), FPAR(1), HIST(1), UMCURP(1)         M42.4987
      DIMENSION  NODVO1(1), NODVO2(1), JCLTAC(1)                        M42.4988
      DIMENSION  JTYPE(1), NODOM(1), JTMTAC(1)                          M42.4989
      DIMENSION  THETAM(1), REAMDU(1), REAMQU(1)                        M42.4990
      DIMENSION  NPPAIR(1), ROTMOM(1), NCLD(1), NCLQ(1)                 M42.4991
      DIMENSION  EPSOM(1), DCOEF(1), KCOIL(1), UMOUTP(1)                M42.4992
      DIMENSION  VOLTUM(1), ANGLUM(1), NODFUM(1), NODMUM(1)             M42.4993
      DIMENSION  ZTHS3(3,3),ZTHS3I(3,3),FJM(4,4),FJV(4)                 M42.4994
      DIMENSION  DUMMA1(3,3), DUMMA2(3,3), GMAT(6,7)                    M42.4995
      COMMON /UMLOCL/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,               M42.4996
     1 N12,N17,N18,N19,N20, D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,         M42.4997
     2 D12,D13,D14,D15,D16,D17,D18, LFIM3, LFIM4I, NCOMCL,              M42.4998
     3 NCOMUM, KCLD1, KCLQ1,KCLF,NMINUM,                                M42.4999
     4 LOPSS1, LOPSS2, LOPSS4, LOPSS8, LOPSS9, LOPSS10, slip            M42.5000
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'SPACE2.FOR;/NOLIST'                                           
      INCLUDE  'UMDECK.FOR;/NOLIST'                                           
C   "FIRST PART" OF UMRENU CODE IS ENTERED TWICE IF LOAD-FLOW           M42.5005
C       IS REQUESTED (LOOPSS(10) = 1 OR 2). THIS IS NEEDED TO           M42.5006
C       LET THE LOAD-FLOW DO ITS WORK TO ADJUST THE SOURCES.            M42.5007
C       HOWEVER, IF UM TYPE-3 IS PRESENT, THE FIRST PART WILL           M42.5008
C       BE ENTERED ONE ADDITIONAL PASS PRIOR TO THE TWO                 M42.5009
C       MENTIONED PASSES IN ORDER TO FIND THE THEV PARAMETERS OF        M42.5010
C       THE UM TYPE-3 EXCITATION CIRCUITS (LOOPSS(10) = 3).             M42.5011
C.  ....... CODE FOR UM STEADY-STATE INITIALIZATION ............        M42.5012
C. "FIRST PART" OF CODE : SET UP OF VECTORS ********************        M42.5013
C     NOTE: THIS PART TILL STMT 1400 IS ONLY ENTERED IF ISTART=0        M42.5014
      IF (LOOPSS(10) .GT. 1) GO TO 1400                                 M42.5015
      IF (ISTART .GT. 0) RETURN                                         M42.5016
      LOOPSS(9) = 0                                                     M42.5017
      IMACH = 0                                                         M42.5018
C   RENUMBERING MECHANICAL NODES :                                      M42.5019
      DO 1230 I = 1,NUMUM                                               M42.5020
      N9 = NODOM(I)                                                     M42.5021
      NODOM(I) = NORDER(N9)                                             M42.5022
 1230 CONTINUE                                                          M42.5023
      DO 1290  I=1, NUMUM                                                       
      KCL = KCOIL(I)                                                    M42.5025
      IF (JTMTAC(I) .GT. 0) GO TO 1250                                          
C. FIND THE UM'S CONNECTED TO SAME MECHANICAL NETWORK :                 M42.5027
C    (INITIALIZE JCLTAC(KCL AND KCL+1) AND IDENTIFYING NODMUM)          M42.5028
 1245 N10 = NODOM(I)                                                            
      N10 = KSSFRQ(N10)                                                         
      IF (N10 .LT. 0) GO TO 1250                                        M42.5031
      N11 = N10                                                                 
      N1 = 0                                                                    
 1246 N1 = N1 + 1                                                       M42.5034
      NDUM(N1) = N10                                                            
      N10 = KPSOUR(N10)                                                         
      IF (N10 .NE. N11) GO TO 1246                                      M42.5037
      IF (N1 .EQ. 1) GO TO 1250                                                 
      IF (N1 .LE. 40) GO TO 81246                                               
      WRITE (LUNIT6,81245) N1,I                                         M42.5040
81245 FORMAT(/,23H ERROR STOP. THERE ARE ,I2, 8H TYPE-14,               M42.5041
     1         41H SOURCES PRESENT IN MECHANICAL NETWORK OF,            M42.5042
     2   10H UM NUMBER, I4, 28H.  THIS NUMBER OVERFLOWS THE             M42.5043
     3  ,/, 42H LIMIT OF 40. INCREASING THIS LIMIT CAN BE,              M42.5044
     4         42H DONE BY INCREASING THE DIMENSION OF ARRAY,           M42.5045
     5         41H "NDUM" WHICH IS DIMENSIONED IN "UMDECK".)            M42.5046
      CALL STOPTP                                                       M42.5047
81246 NUMMEC = 0                                                                
      DO 1248 J = 1,NUMUM                                                       
      IF (J .EQ. I) GO TO 1248                                          M42.5050
      KCLJ = KCOIL(J)                                                   M42.5051
      N4 = NODOM(J)                                                             
      N4 = KSSFRQ(N4)                                                   M42.5053
      IF (N4 .LT. 0) GO TO 1248                                         M42.5054
81247 DO 1247 N2 = 1,N1                                                         
      IF (NDUM(N2) .NE. N4) GO TO 1247                                  M42.5056
      NUMMEC = NUMMEC + 1                                                       
      IF (NUMMEC .EQ. 1) JCLTAC(KCL) = J                                M42.5058
      IF (NUMMEC .EQ. 2) JCLTAC(KCL+1) = J                              M42.5059
      IF (J .GT. I) NODMUM(J) = NODMUM(I)                                       
      GO TO 1248                                                                
 1247 CONTINUE                                                          M42.5062
 1248 CONTINUE                                                          M42.5063
      IF (NUMMEC .LE. 2) GO TO 1250                                     M42.5064
      N2 = NUMMEC - 1                                                   M42.5065
      WRITE (LUNIT6,1249) I,N2                                          M42.5066
 1249 FORMAT(/,22H ERROR STOP. UM NUMBER, I4,13H IS CONNECTED,          M42.5067
     1         17H MECHANICALLY TO ,I2,17H OTHER UM'S. THIS,            M42.5068
     2         44H CAN NOT BE HONORED BECAUSE AT THE MOST ONLY          M42.5069
     3   ,/,   45H 3 UM'S CAN BE CONNECTED TO A SAME MECHANICAL,        M42.5070
     4         41H NETWORK. YOU MAY RESOLVE THIS PROBLEM BY,            M42.5071
     5         42H INSERTING STUB LINES IN ORDER TO SEPERATE)           M42.5072
      WRITE (LUNIT6,81249)                                              M42.5073
81249 FORMAT(23H THE MECHANICAL NETWORK,                                M42.5074
     1       25H FROM THE REMAINING UM'S.)                              M42.5075
      CALL STOPTP                                                       M42.5076
C. DEFAULT VALUE FOR EPSOM(I) :                                         M42.5077
 1250 IF (EPSOM(I) .NE. 0.0) GO TO 1260                                 M42.5078
      EPSOM(I) = 0.001                                                  M42.5079
      IF (JTYPE(I) .EQ. 13) GO TO 1255                                  M42.5080
      IF (INPU .EQ. 1) GO TO 1260                                       M42.5081
 1255 EPSOM(I) = 0.001 * ROTMOM(I)/NPPAIR(I)                            M42.5082
 1260 IF (JTYPE(I) .GT. 2 .AND. JTYPE(I) .LT. 8) IMACH=1                M42.5083
      IF (JTYPE(I) .EQ. 3 .AND. LOOPSS(10) .EQ. 1) LOOPSS(10)=3         M42.5084
C   RENUMBERING ELECTRICAL NODES :                                      M42.5085
      IF (JTMTAC(I) .GE. 0) GO TO 1262                                  M42.5086
      N9 = - JTMTAC(I)                                                          
      JTMTAC(I) = - NORDER(N9)                                          M42.5088
 1262 IF (JTYPE(I) .NE. 13) GO TO 1270                                  M42.5089
      IF (FPAR(KCL+3) .GE. 0.0) GO TO 1270                                      
      N5 = FPAR(KCL+4)                                                          
      FPAR(KCL+4) = NORDER(N5)                                          M42.5092
      N5 = FPAR(KCL+5)                                                          
      FPAR(KCL+5) = NORDER(N5)                                          M42.5094
 1270 N2 = 3 * (I - 1)                                                  M42.5095
      DO 1280 N1 = 1,3                                                  M42.5096
      N3 = UMCURP(N1+N2)                                                        
      N3 = NORDER(N3)                                                   M42.5098
      UMCURP(N1+N2) = N3                                                M42.5099
 1280 CONTINUE                                                          M42.5100
 1290 CONTINUE                                                          M42.5101
      IF (IMACH .EQ. 1 .AND. LOOPSS(8) .EQ. 1) LOOPSS(9)=1              M42.5102
      DO 1327  I=1, NCLTOT                                              M42.5103
      N8 = NODVO1(I)                                                    M42.5104
      NODVO1(I) = NORDER(N8)                                            M42.5105
      N9 = NODVO2(I)                                                    M42.5106
 1327 NODVO2(I) = NORDER(N9)                                            M42.5107
C  RENUMBERING IN CASE OF TACS TRANSFER WITH SM TYPE-59 DATA :          M42.5108
      IF (NSMACH .EQ. 0) GO TO 1340                                     M42.5109
      N5 = UMOUTP(NUMOUT+1)                                             M42.5110
      IF (N5 .NE. -9999.0) GO TO 1340                                   M42.5111
      N5 = NUMOUT + 4                                                   M42.5112
      N6 = UMOUTP(NUMOUT+3) - 2                                         M42.5113
 1330 IF (N5 .GT. N6) GO TO 1338                                        M42.5114
      N8 = UMOUTP(N5)                                                   M42.5115
      IF (N8 .EQ. -2) GO TO 1332                                        M42.5116
      IF (N8 .EQ. -3) GO TO 1332                                        M42.5117
      IF (N8 .EQ. -5) GO TO 1332                                        M42.5118
      IF (N8 .GT. -298) GO TO 1334                                      M42.5119
 1332 N9 = UMOUTP(N5+1)                                                 M42.5120
      UMOUTP(N5+1) = NORDER(N9)                                         M42.5121
      N5 = N5 + 3                                                       M42.5122
      IF (N8 .LE. -299) N5 = N5 + 5                                     M42.5123
      GO TO 1330                                                        M42.5124
 1334 N5 = N5 + 1                                                       M42.5125
      GO TO 1330                                                        M42.5126
 1338 N5 = UMOUTP(NUMOUT+3)                                             M42.5127
      IF (IPRSUP .GE. 1)                                                M42.5128
     1 WRITE (LUNIT6,1339) (UMOUTP(I),I=1,N5)                           M42.5129
 1339 FORMAT(/,40H UM OUTPUT TABLE UMOUTP(NUMOUT+NSMTAC+3),             M42.5130
     1         40H WITH SOME ENTRIES RENUMBERED BECAUSE OF,             M42.5131
     2         38H TACS INTERFACE FOR SM-59 DATA INPUT :                M42.5132
     3    ,/,6(6X,E14.5)/(6(6X,E14.5)) )                                M42.5133
 1340 IF (IPRSUP .LT. 1) GO TO 1350                                     M42.5134
      WRITE (LUNIT6, 1341)                                              M42.5135
 1341 FORMAT (/,29H UM NODES AFTER RENUMBERING :)                       M42.5136
      WRITE (LUNIT6, 1345)  ( NODVO1(I), I=1, NCLTOT )                  M42.5137
 1345 FORMAT (19H NODVO1(1:NCLTOT) :,  20I5  )                          M42.5138
      WRITE (LUNIT6, 1346)  ( NODVO2(I), I=1, NCLTOT )                  M42.5139
 1346 FORMAT (19H NODVO2(1:NCLTOT) :,  20I5  )                          M42.5140
      N5 = 3 * NUMUM                                                    M42.5141
      WRITE (LUNIT6, 1349)  ( UMCURP(I), I=1, N5)                       M42.5142
 1349 FORMAT (19H UMCURP(1:3NUMUM) :,  20F5.1)                          M42.5143
      WRITE(LUNIT6,1347) (KCOIL(I),I=1,NUMUM)                           M42.5144
 1347 FORMAT (19H KCOIL(1:NUMUM)   :,20I5)                              M42.5145
      WRITE(LUNIT6,1348) (NODOM(I),I=1,NUMUM)                           M42.5146
 1348 FORMAT (19H NODOM(1:NUMUM)   :,20I5)                              M42.5147
C.   IF USER DOES NOT REQUEST FOR AUTOMATIC INITIALIZATION :            M42.5148
 1350 IF (INITUM .EQ. 0) RETURN                                         M42.5149
C.   SETTING OF ALL FLAGS (IF NOT INCLUDED, THEN THEY ARE 0):           M42.5150
      LOOPSS(3) = KCONST                                                M42.5151
      LOOPSS(5) = IBR                                                   M42.5152
      LOOPSS(12) = LOOPSS(5)                                            M42.5153
      LOOPSS(7) = KSSOUT                                                M42.5154
      HIST(KCL+1) = NUMSUB                                              M42.5155
      KSSOUT = 0                                                        M42.5156
C  ******* POINT OF ENTRANCE AT SECOND PASS THRU "FIRST PART".          M42.5157
 1400 IF (LOOPSS(10) .NE. 3) GO TO 1480                                         
      IF (ISTEP .EQ. -4567) GO TO 1480                                          
      LOOPSS(10) = 1                                                            
      ISTEP = - 4567                                                            
      LFIM3 = 1                                                                 
 1480 IF (LOOPSS(10) .NE. 2) GO TO 1500                                 M42.5163
      KCONST = LOOPSS(3)                                                M42.5164
      IBR = LOOPSS(12)                                                  M42.5165
      IT = - NR(IBR)                                                    M42.5166
      NUMSUB = HIST(KCL+1)                                              M42.5167
 1500 N15 = 0                                                           M42.5168
      NCOMCL = 0                                                        M42.5169
      NCOMUM = 0                                                                
C   ************** START MACHINE DO-LOOP ***************                M42.5171
      DO 2200 K = 1,NUMUM                                               M42.5172
      LFIM4I = 0                                                                
      KCL = KCOIL(K)                                                    M42.5174
      NCL = 3 + NCLD(K) + NCLQ(K)                                       M42.5175
      KCLD1 = KCL + 3                                                           
      KCLQ1 = KCLD1 + NCLD(K)                                                   
      IF (JTYPE(K) .EQ. 4) NCL = NCL + 1                                M42.5178
      KCLE = KCL - 1 + NCL                                                      
      IF (K .EQ. 1) KSUBUM = NUMSUB + 1                                 M42.5180
      IMACH = 1                                                                 
      IF (JTYPE(K) .LT. 3) IMACH = 0                                    M42.5182
      IF (JTYPE(K) .GT. 7) IMACH = 0                                    M42.5183
      IF (IMACH .EQ. 1) SLIP = VOLTUM(K)/100.0                          M42.5184
      IF (LOOPSS(10) .NE. 3) GO TO 1900                                 M42.5185
      IF (IPRSUP .GE. 1) WRITE(LUNIT6,1850)                             M42.5186
 1850 FORMAT(/,40H ***************************************,             M42.5187
     1         45H PREPARING THEVENIN CALCULATION FOR UM TYPE-3,        M42.5188
     3         44H EXCITATION CIRCUITS DUE TO LD-FLOW REQUEST:)         M42.5189
      GO TO 2008                                                        M42.5190
 1900 IF (IPRSUP .GE. 1) WRITE (LUNIT6,1902)                            M42.5191
 1902 FORMAT(/,40H ***************************************,             M42.5192
     1         38H MOMENTARY CREATION OF ADDITIONAL EMTP,               M42.5193
     2         38H ELEMENTS (PERMANENT BRANCHES IF POWER                M42.5194
     3   ,/,   40H ***************************************,             M42.5195
     4         38H COILS ARE NON-COMP.) FOR STEADY-STATE,               M42.5196
     5         16H CALCULATIONS OF)                                     M42.5197
      IF (IPRSUP .GE. 1) WRITE (LUNIT6,1904) K                          M42.5198
 1904 FORMAT(10H UM NUMBER,I4,2H :,28X,18HNODE TO NODE   IBR,           M42.5199
     1        4X,2HIT,8X,6HTR(IT),8X,6HTX(IT),2X,6HKCONST,              M42.5200
     2        9X,5HSFREQ,9X,5HCREST)                                    M42.5201
      IF (JTYPE(K) .EQ. 4 .AND. LOOPSS(10) .EQ. 1) LFIM4I = 1           M42.5202
      IF (JTYPE(K) .EQ. 4 .AND. LOOPSS(10) .EQ. 2) LFIM4I = 2           M42.5203
C  BRING THE IM TYPE-4 EXCITATION  SOURCES BACK TO SLIP-FREQ AT         M42.5204
C       PASS FOR WHICH LOOPSS(10) = 2 AND UM = IM TYPE-4 :              M42.5205
C  NOTE : THIS IS NOT NECESSARY FOR THE ROTOR RESISTANCES               M42.5206
C         BECAUSE OF THE "RESTORE" FEATURE FOR R,L,C BRANCHES.          M42.5207
      IF (LFIM4I .NE. 2) GO TO 2000                                     M42.5208
      IF (LOOPSS(10) .NE. 2) GO TO 2000                                 M42.5209
      D3 = SLIP                                                         M42.5210
      IF (D3 .LT. 0.0) D3 = - D3                                        M42.5211
      N19 = FPAR(KCL+1)                                                         
      N18 = NCL - 3                                                             
      N20 = N19 - 1 + N18                                               M42.5214
      DO 1970 N13 = 1,N18                                                       
      N14 = KCL + 2 + N13                                               M42.5216
      N2 = NODVO1(N14)                                                  M42.5217
      IF (N2 .EQ. 1) N2 = NODVO2(N14)                                   M42.5218
      IF (N2 .EQ. 1) GO TO 1970                                                 
      N3 = KSSFRQ(N2)                                                   M42.5220
      N4 = N3                                                           M42.5221
      IF (N3 .GE. N19 .AND. N3 .LE. N20) THEN                           M42.5222
      SFREQ(N3) = D3 * SFREQ(N3)                                        M42.5223
      END IF                                                            M42.5224
 1950 IF (N3 .GE. N19 .AND. N3 .LE. N20) GO TO 1960                     M42.5225
      IF (NODE(N3) .LT. 0) GO TO 1960                                           
      CREST(N3) = - SLIP * CREST(N3)                                    M42.5227
      SFREQ(N3) = D3 * SFREQ(N3)                                        M42.5228
      IF (SLIP .LT. 0.0) TIME1(N3) = - TIME1(N3)                        M42.5229
      D4 = TIME1(N3) * 360.0/TWOPI                                      M42.5230
      N6 = NODE(N3)                                                     M42.5231
      IF (N13 .EQ. 1) WRITE(LUNIT6,1952) K                              M42.5232
      IF (N13 .EQ. 1) WRITE(LUNIT6,1954)                                M42.5233
      WRITE(LUNIT6,1956) BUS(N6),CREST(N3),SFREQ(N3),D4                 M42.5234
      IF (N13 .EQ. N18) WRITE(LUNIT6,1958)                              M42.5235
 1952 FORMAT(/,45H ON COMPLETION OF THE LOAD-FLOW CALCULATIONS,,        M42.5236
     1         45H THE ARGUMENTS OF THE TYPE-14 SOURCES USED TO,        M42.5237
     2         42H REPRESENT THE EXTERNAL VOLTAGE SOURCES IN            M42.5238
     3  ,/,    36H THE EXCITATION NETWORK OF UM NUMBER,I4,              M42.5239
     4         35H,  ARE ADJUSTED BY THE PROGRAM AS :)                  M42.5240
 1954 FORMAT(9H BUS NAME,2X,14H  AMPLITUDE(V),2X,                       M42.5241
     1      14H FREQUENCY(HZ),2X,14H   ANGLE(DEGR))                     M42.5242
 1956 FORMAT(3X,A6,3E16.5)                                              M42.5243
 1958 FORMAT(/)                                                         M42.5244
 1960 N3 = KPSOUR(N3)                                                           
      IF (N3 .NE. N4) GO TO 1950                                        M42.5246
 1970 CONTINUE                                                          M42.5247
C  ERROR STOP IF LOAD FLOW OF IM WHICH IS NOT OF TYPE 4 AND NOT         M42.5248
C    SYMMETRICAL ROUND ROTOR :                                          M42.5249
 2000 IF (LOOPSS(10) .EQ. 0) GO TO 2008                                         
      IF (LFIM4I .EQ. 0) GO TO 2008                                             
      IF (JTYPE(K) .NE. 4) GO TO 2002                                   M42.5252
      IF (REAMDU(K) .EQ. REAMQU(K)) GO TO 2008                          M42.5253
 2002 WRITE(LUNIT6,2004)                                                M42.5254
 2004 FORMAT(/,44H ERROR STOP . YOU HAVE REQUESTED A LOAD-FLOW,         M42.5255
     1         45H OF A NETWORK CONTAINING AN INDUCTION MACHINE,        M42.5256
     2         42H WHICH IS EITHER NOT OF TYPE 4 OR WHICH IS            M42.5257
     3      ,/,43H OF TYPE 4 BUT IS NOT A SYMMETRICAL MACHINE,          M42.5258
     4         45H WITH A ROUND ROTOR. THE CURRENT EMTP VERSION,        M42.5259
     5         42H IS NOT CAPABLE OF HONORING YOUR LOAD FLOW            M42.5260
     6      ,/, 9H REQUEST.)                                            M42.5261
      CALL STOPTP                                                               
C  COMPENSATION CODE ************************************* :            M42.5263
 2008 IF (LOOPSS(10) .EQ. 1) GO TO 2048                                 M42.5264
      IF (IMACH .NE. 1) GO TO 2048                                      M42.5265
      IF (IPRSUP .GE. 1) WRITE (LUNIT6,2010) K,NUMSUB                   M42.5266
 2010 FORMAT(44H * * * * * * * * * * * * * * * * * * * * * *,           M42.5267
     1       37H BEGIN COMPENSATION FOR IND.MACHINES:,                  M42.5268
     2       10H UM NUMBER,I4,3X, 9H NUMSUB =, I4)                      M42.5269
 2017 N1 = 0                                                            M42.5270
      N18 = 0                                                           M42.5271
      N2 = NCOMCL                                                       M42.5272
      IF (LOOPSS(8) .EQ. 1) GO TO 2021                                  M42.5273
      N5 = KCOIL(K)                                                     M42.5274
      N9 = N5 + 2                                                       M42.5275
 2018 NUMSUB = NUMSUB + 1                                               M42.5276
      ISUBEG(NUMSUB) = N15 + 1                                          M42.5277
      DO 2020 J = N5,N9                                                 M42.5278
      IF (NODVO1(J) .EQ. NODVO2(J)) GO TO 2020                          M42.5279
      N15 = N15 + 1                                                     M42.5280
      NCOMCL = NCOMCL + 1                                               M42.5281
      N18 = N18 + 1                                                     M42.5282
      KSUB(N15) = NODVO1(J)                                             M42.5283
      MSUB(N15) = NODVO2(J)                                             M42.5284
 2020 CONTINUE                                                          M42.5285
      IF (N1 .EQ. 1) GO TO 2025                                         M42.5286
 2021 N7 = KCLE - KCOIL(K) + 1                                          M42.5287
      N9 = KCLE                                                         M42.5288
      IF (N7 .GT. 6) N9 = KCOIL(K) + 5                                  M42.5289
      N5 = KCOIL(K) + 3                                                 M42.5290
      N1 = 1                                                            M42.5291
      IF (LOOPSS(8) .EQ. 1) GO TO 2022                                  M42.5292
      IF (NCOMCL .NE. N2) GO TO 2022                                    M42.5293
      ISUBEG(NUMSUB) = ISUBEG(NUMSUB) - N15 - 1                         M42.5294
      NUMSUB = NUMSUB -1                                                M42.5295
 2022 N18 = 0                                                           M42.5296
      GO TO 2018                                                        M42.5297
 2025 IF (N18 .NE. 0) GO TO 2030                                        M42.5298
      ISUBEG(NUMSUB) = ISUBEG(NUMSUB) - N15 - 1                         M42.5299
      NUMSUB = NUMSUB - 1                                               M42.5300
 2030 IF (N7 .LE. 6) GO TO 2045                                         M42.5301
      N5 = KCOIL(K) + 6                                                 M42.5302
      DO 2040 J = N5,KCLE                                               M42.5303
      IF (NODVO1(J) .EQ. NODVO2(J)) GO TO 2040                          M42.5304
      NCOMCL = NCOMCL + 1                                               M42.5305
      NUMSUB = NUMSUB + 1                                               M42.5306
      ISUBEG(NUMSUB) = N15 + 1                                          M42.5307
      N15 = N15 + 1                                                     M42.5308
      KSUB(N15) = NODVO1(J)                                             M42.5309
      MSUB(N15) = NODVO2(J)                                             M42.5310
 2040 CONTINUE                                                          M42.5311
 2045 IF (IPRSUP .GE. 1) WRITE (LUNIT6,2047) NUMSUB                     M42.5312
 2047 FORMAT(44H * * * * * * * * * * * * * * * * * * * * * *,           M42.5313
     1       30H * * * * * * * * * * * * * * *,                         M42.5314
     2       42H * END COMPENSATION FOR IND.MACH. NUMSUB =, I4)         M42.5315
 2048 NCOMUM = NCOMCL                                                   M42.5316
      IF (LOOPSS(10) .EQ. 3) GO TO 2087                                 M42.5317
C.  SET TYPE 14 CURRENT SOURCES AND PARALLEL R FOR POWER COILS:         M42.5318
C     NOTE : IF LOOPSS(8)=0, MOMENTARY R FOR SM AND DM ONLY TILL        M42.5319
C              COMPLETION OF ALL STEADY-STATE UMRENU PASSES.            M42.5320
C            IF LOOPSS(8)=1, PERMANENT BRANCHES FOR ALL UM TYPES        M42.5321
      IF (IMACH .EQ. 1) GO TO 2050                                      M42.5322
      IF (LOOPSS(10) .EQ. 1) D10 = VOLTUM(K)                            M42.5323
      IF (LOOPSS(10) .EQ. 2) VOLTUM(K) = CREST(KCONST+1)                M42.5324
      IF (LOOPSS(10) .EQ. 2) ANGLUM(K) = TIME1(KCONST+1)*360.0/T        M42.5325
 2050 DO 2080 N1 = 1,3                                                  M42.5326
      N2 = N1 + KCOIL(K) - 1                                            M42.5327
      N4 = 3 * (K - 1)                                                  M42.5328
      IF (NODVO1(N2) .EQ. NODVO2(N2)) GO TO 2080                        M42.5329
      IF (LOOPSS(8) .EQ. 0 .AND. IMACH .EQ. 1) GO TO 2055               M42.5330
      CALL IBRINC                                                       VAX.  72
      IT = IT + 1                                                       M42.5332
      KBUS(IBR) = NODVO1(N2)                                            M42.5333
      MBUS(IBR) = NODVO2(N2)                                            M42.5334
      LENGTH(IBR) = 1                                                   M42.5335
      NR(IBR) = - IT                                                    M42.5336
      TR(IT) = EPSILN                                                   M42.5337
      TX(IT) = 0.0                                                      M42.5338
      C(IT) = 0.0                                                       M42.5339
      IF (N1 .EQ. 3) HIST(KCL) = IBR                                    M42.5340
C  LOAD-FLOW PREPARATION OF MAIN INDUCTANCE OF IM TYPE-4 :              M42.5341
C  NOTE : AFTER COMPLETION OF PROCESSING THE LOAD-FLOW, THESE           M42.5342
C         BRANCHES SERVE AS THE POWER RESISTANCE BRANCHE ACROSS         M42.5343
C         THE TERMINALS OF THE MACHINE. THE FULLY COMPENSATED IM        M42.5344
C         WILL BE PROVIDED WITH THESE BRANCHES ONLY DURING THE          M42.5345
C         LOAD-FLOW PROCESS AND ARE CREATED FURTHER DOWN. THEY          M42.5346
C         WILL BE WIPED OUT AUTOMATICALLY AFTER COMPLETION OF           M42.5347
C         THE LD-FLOW SINCE IN REENTERING THE "FIRST PART" OF           M42.5348
C         UMRENU IBR WILL BE RESTORED TO ITS VALUE AS DETERMINED        M42.5349
C         IN UMDATA.                                                    M42.5350
      IF (LFIM4I .NE. 1) GO TO 2053                                             
      TR(IT) = 0.0                                                      M42.5352
      TX(IT) = REAMQU(K) * 1.0D+3                                       M42.5353
      IF (XOPT .NE. 0.0) TX(IT) = REAMQU(K) * TWOPI * XOPT              M42.5354
      IF (IPRSUP .GE. 1) THEN                                           M42.5355
      WRITE(LUNIT6,2052) KBUS(IBR),MBUS(IBR),IBR,IT,TR(IT),             M42.5356
     1 TX(I)                                                            M42.5357
      END IF                                                            M42.5358
 2052 FORMAT(10H *********,                                             M42.5359
     1       32H IM MAIN IND BRANCHE FOR LD-FLOW,2X,I4,                 M42.5360
     2        4X,I4,2I6,2E14.5)                                         M42.5361
      GO TO 2055                                                        M42.5362
C   SET PARALLEL RESISTANCES FOR SM AND DM AND PREDICTED IM :           M42.5363
 2053 IF (KBUS(IBR) .NE. 1 .AND. MBUS(IBR) .NE. 1) TR(IT)=EPSILN        M42.5364
      IF (LOOPSS(10) .EQ. 1) TR(IT) = 1.0D+6                            M42.5365
      IF (IMACH .EQ. 1) TR(IT) = 1.0                                    M42.5366
      IF (IPRSUP .GE. 1)                                                M42.5367
     1 WRITE (LUNIT6,2054) KBUS(IBR),MBUS(IBR),IBR,IT,TR(IT),           M42.5368
     2                     TX(IT)                                       M42.5369
 2054 FORMAT(10H *********,                                             M42.5370
     1       31H PARALL RESIST. FOR POWER COILS, 3X,I4,                 M42.5371
     2        4X,I4,2I6,2E14.5)                                         M42.5372
      IF (LOOPSS(10) .EQ. 1) GO TO 2055                                 M42.5373
      D10 = VOLTUM(K)/TR(IT)                                            M42.5374
C   SET SOURCES REPRESENTING THE UM POWER COILS :                       M42.5375
 2055 IF (IMACH .EQ. 1) D10 = 0.0                                       M42.5376
 2056 DO 2070 N3 = 1,2                                                  M42.5377
      IF (N3 .EQ. 2) GO TO 2060                                         M42.5378
      IF(NODVO1(N2) .EQ. 1) GO TO 2070                                  M42.5379
      KCONST = KCONST + 1                                               M42.5380
      IF ( KCONST .LE. LEXCT )  GO TO 6785                              M42.5381
      WRITE (LUNIT6, 6783)  LEXCT                                       M42.5382
 6783 FORMAT ( 38H OVERFLOW LIST 4 IN "UMRENU".  LEXCT =,               M42.5383
     1  I5,    18H     STOP LOCALLY.     )                              M42.5384
      CALL STOPTP                                                       M42.5385
 6785 N10 = NODVO1(N2)                                                  M42.5386
      NODE(KCONST) = - NODVO1(N2)                                       M42.5387
      IF (LOOPSS(10) .NE. 1) GO TO 2058                                 M42.5388
      IF (IMACH .EQ. 1) GO TO 2058                                      M42.5389
      NODE(KCONST) = UMCURP(N4+N1)                                      M42.5390
 2058 CREST(KCONST) = D10                                               M42.5391
      GO TO 2062                                                        M42.5392
 2060 IF (NODVO2(N2) .EQ. 1) GO TO 2070                                 M42.5393
      KCONST = KCONST + 1                                               M42.5394
      IF ( KCONST .LE. LEXCT )  GO TO 6786                              M42.5395
      WRITE (LUNIT6, 6783)  LEXCT                                       M42.5396
      CALL STOPTP                                                       M42.5397
 6786 N10 = NODVO2(N2)                                                  M42.5398
      NODE(KCONST) = - NODVO2(N2)                                       M42.5399
      IF (LOOPSS(10) .NE. 1) GO TO 2061                                 M42.5400
      IF (IMACH .EQ. 1) GO TO 2061                                      M42.5401
      NODE(KCONST) = UMCURP(N4+N1)                                      M42.5402
 2061 CREST(KCONST) = - D10                                             M42.5403
 2062 IFORM(KCONST) = 14                                                M42.5404
      IF (LFIM3 .EQ. 1 .AND. N1 .EQ. 1) HIST(KCL+2) = KCONST            M42.5405
      D1 = ANGLUM(K) * TWOPI/360.0                                      M42.5406
      IF (N1 .NE. 2) GO TO 2064                                         M42.5407
      D2 = D1 - TWOPI/3.0                                               M42.5408
      IF (JTYPE(K) .EQ. 2) D2 = D1                                      M42.5409
      D1 = D2                                                           M42.5410
      GO TO 2065                                                        M42.5411
 2064 IF (N1 .NE. 3) GO TO 2065                                         M42.5412
      D2 = D1 + TWOPI/3.0                                               M42.5413
      IF (JTYPE(K) .EQ. 2) D2 = D1 + TWOPI/4.0                          M42.5414
      IF (JTYPE(K) .EQ. 5) D2 = D1 + TWOPI/4.0                          M42.5415
      D1 = D2                                                           M42.5416
 2065 IF (IMACH .EQ. 1) D1 = 0.0                                        M42.5417
      IF (LOOPSS(10) .EQ. 2) GO TO 2066                                 M42.5418
      TIME1(KCONST) = D1                                                M42.5419
      TSTART(KCONST) = - 1.0                                            M42.5420
      TSTOP(KCONST) = 0.0                                               M42.5421
      N10 = KSSFRQ(N10)                                                 M42.5422
      IF (N10 .GT. 0) GO TO 92066                                       M42.5423
      SFREQ(KCONST) = ROTMOM(K)/TWOPI                                   M42.5424
      GO TO 2066                                                        M42.5425
92066 SFREQ(KCONST) = SFREQ(N10)                                        M42.5426
C   FOR LOAD-FLOW THE SM ALWAYS PROVIDE A VOLTAGE SOURCE TO             M42.5427
C    REPRESENT MACHINE, WHEREAS IM PROVIDES A DUMMY CURRENT             M42.5428
C    SOURCE WHICH WILL BE INITIALIZED AT A LATER PASS AFTER             M42.5429
C    DESTRUCTION OF THE IM LOAD-FLOW NETWORK CONFIGURATION.             M42.5430
 2066 IF (LOOPSS(10) .EQ. 0) GO TO 2068                                 M42.5431
      IF (IMACH .EQ. 1) GO TO 2068                                      M42.5432
      IF (IPRSUP .GE. 1)                                                M42.5433
     1 WRITE (LUNIT6,2067) NODE(KCONST),KCONST,SFREQ(KCONST),           M42.5434
     2                     CREST(KCONST)                                M42.5435
 2067 FORMAT(10H *********,                                             M42.5436
     1       30H VOLTAGE SOURCE FOR POWER COIL, 4X,I4,50X,              M42.5437
     2         I6,E14.5,1X,E13.5)                                       M42.5438
      GO TO 2070                                                        M42.5439
 2068 IF (IPRSUP .GE. 1)                                                M42.5440
     1 WRITE (LUNIT6,2069) NODE(KCONST),KCONST,SFREQ(KCONST),           M42.5441
     2                     CREST(KCONST)                                M42.5442
 2069 FORMAT(10H *********,                                             M42.5443
     1       30H CURRENT SOURCE FOR POWER COIL, 4X,I4,50X,              M42.5444
     2         I6,E14.5,1X,E13.5)                                       M42.5445
 2070 CONTINUE                                                          M42.5446
C   FOR SM AND DM : MAKE LEAKAGE INDUCTANCE ZERO IN EMTP                M42.5447
C    NETWORK FOR THIS NEXT ROUND OF SS CALCULATION BECAUSE              M42.5448
C    USER SUPPLIED TERMINAL VOLTAGE SPECIFICATION NEEDS TO              M42.5449
C    BE MATCHED. ON THE NEXT ROUND THEY WILL BE RESTORED.               M42.5450
C    NOTE: IF LOAD-FLOW IS REQUESTED, THIS ZEROING WILL BE DONE         M42.5451
C          ON THE SECOND PASS THROUGH THE FIRST PART OF THE             M42.5452
C          UMRENU CODE.                                                 M42.5453
C          IF IND MACH IS PRESENT AND NO LOAD-FLOW REQUESTED , T        M42.5454
C          THEN THIS PROCEDURE IS TO BE DONE IF LOOPSS(9) = 2,          M42.5455
C          I.E. IN "SECOND PART" OF THE UMRENU CODE.                    M42.5456
      IF (LOOPSS(10) .EQ. 1) GO TO 2076                                         
      IF (IMACH .EQ. 1) GO TO 2076                                              
      IF (LOOPSS(9) .NE. 0) GO TO 2076                                          
      N10 = FPAR(KCL) + N1 - 1                                                  
      N5 = - NR(N10)                                                            
      IF (N5 .LT. 0) N5 = - N5                                          M42.5462
      TX(N5) = EPSILN * 1.0D+3                                          M42.5463
      IF (XOPT .NE. 0.0) TX(N5) = EPSILN * TWOPI * XOPT                 M42.5464
      IF (IPRSUP .GE. 1)                                                M42.5465
     1 WRITE (LUNIT6,2074) KBUS(N10),MBUS(N10),N10,N5,                  M42.5466
     2                     TR(N5),TX(N5)                                M42.5467
 2074 FORMAT (32H *********  LEAKAGE IND. CHANGE:,                      M42.5468
     1        12X,I4,4X,I4,2I6,2E14.5)                                  M42.5469
C  LOAD-FLOW PREPARATION OF IM TYPE-4 POWER SERIES BRANCHE :            M42.5470
 2076 IF (LFIM4I .NE. 1) GO TO 2080                                     M42.5471
      N10 = FPAR(KCL) + N1 - 1                                                  
      N5 = - NR(N10)                                                            
      IF (N5 .LT. 0) N5 = - N5                                          M42.5474
      TR(N5) = 1.0/GPAR(KCL+2)                                          M42.5475
      IF (IPRSUP .GE. 1)                                                M42.5476
     1 WRITE (LUNIT6,2078) KBUS(N10),MBUS(N10),N10,N5,                  M42.5477
     2                     TR(N5),TX(N5)                                M42.5478
 2078 FORMAT (39H ********* IM POWER BRANCHE FOR LD-FLOW,               M42.5479
     1        5X,I4,4X,I4,2I6,2E14.5)                                   M42.5480
 2080 CONTINUE                                                          M42.5481
C  STORING PERMANENTLY CREATED BRANCHES (IF FULL COMPENSATION IS        M42.5482
C   REQUESTED, THEN ALL CREATED BRANCHES IN UMRENU ARE JUST             M42.5483
C   TEMPORARILY)                                                        M42.5484
      IF (LOOPSS(8) .EQ. 0) GO TO 2081                                  M42.5485
      IF (LOOPSS(10) .NE. 1) LOOPSS(5) = IBR                            M42.5486
C   INITIALIZATION IF INDUCTION MACHINES                                M42.5487
C     NOTE : FOR IM WE CHOOSE THE TORQUE ANGLE = 0.0                    M42.5488
 2081 IF (LOOPSS(10) .EQ. 2) GO TO 2082                                 M42.5489
      IF (IMACH .EQ. 1) THETAM(K) = TWOPI/(4.0*NPPAIR(K))               M42.5490
C.  SET TYPE 14 SOURCES FOR UM = SM & DM EXCITATION COILS               M42.5491
C    (IM EXCITATION COIL SOURCES WERE CREATED IN OVER5)                 M42.5492
 2082 N5 = KCOIL(K) + 3                                                 M42.5493
      IF (IMACH .EQ. 1) GO TO 2087                                      M42.5494
      IF (NODVO1(N5) .EQ. NODVO2(N5)) GO TO 2100                        M42.5495
      DO 2086 N1 = 1,2                                                  M42.5496
      IF (N1 .EQ. 2) GO TO 2083                                         M42.5497
      IF (NODVO1(N5) .EQ. 1) GO TO 2086                                 M42.5498
      KCONST = KCONST + 1                                               M42.5499
      IF ( KCONST .LE. LEXCT )  GO TO 6787                              M42.5500
 6788 WRITE (LUNIT6, 6783)  LEXCT                                       M42.5501
      CALL STOPTP                                                       M42.5502
 6787 NODE(KCONST) = - NODVO1(N5)                                       M42.5503
      GO TO 2084                                                        M42.5504
 2083 IF (NODVO2(N5) .EQ. 1) GO TO 2086                                 M42.5505
      KCONST = KCONST + 1                                               M42.5506
      IF ( KCONST .GT. LEXCT )  GO TO 6788                              M42.5507
      NODE(KCONST) = - NODVO2(N5)                                       M42.5508
 2084 IFORM(KCONST) = 14                                                M42.5509
      CREST(KCONST) = 0.0                                               M42.5510
      TIME1(KCONST) = 0.0                                               M42.5511
      TSTART(KCONST) = - 1.0                                            M42.5512
      TSTOP(KCONST) = 0.0                                               M42.5513
      N10 = NODFUM(K)                                                   M42.5514
      SFREQ(KCONST) = SFREQ(N10)                                        M42.5515
      IF (IPRSUP .GE. 1)                                                M42.5516
     1 WRITE(LUNIT6,2085) NODE(KCONST),KCONST,SFREQ(KCONST),            M42.5517
     2                    CREST(KCONST)                                 M42.5518
 2085 FORMAT(10H *********,                                             M42.5519
     1       31H CURRENT SOURCE FOR EXCIT COILS,3X,I4,50X,              M42.5520
     2         I6,E14.5,1X,E13.5)                                       M42.5521
 2086 CONTINUE                                                          M42.5522
      GO TO 2100                                                        M42.5523
C  TREATMENT OF  EXCITATION SOURCES OF IM :                             M42.5524
 2087 IF (LOOPSS(10) .EQ. 2) GO TO 2100                                 M42.5525
      D3 = SLIP                                                         M42.5526
      IF (D3 .LT. 0.0) D3 = - D3                                        M42.5527
      D1 = D3 * ROTMOM(K)/TWOPI                                         M42.5528
      IF (LFIM4I .NE. 0) D1 = ROTMOM(K)/TWOPI                           M42.5529
      N19 = FPAR(KCL+1)                                                         
      N20 = N19 - 1 + NCLD(K) + NCLQ(K)                                 M42.5531
      IF (JTYPE(K) .EQ. 4) N20 = N19 + 2                                M42.5532
      NMINUM = 10000                                                    M42.5533
      N18 = 0                                                                   
      DO 2094 J = KCLD1,KCLE                                            M42.5535
      N2 = NODVO2(J)                                                    M42.5536
      IF (N2 .EQ. 1) N2 = NODVO1(J)                                     M42.5537
      N3 = KSSFRQ(N2)                                                   M42.5538
      IF (N3 .LT. 0) GO TO 2094                                         M42.5539
      N4 = N3                                                           M42.5540
      SFREQ(N3) = D1                                                    M42.5541
      IF (N3 .GE. N19 .AND. N3 .LE. N20) GO TO 2090                     M42.5542
      N18 = N18 + 1                                                     M42.5543
      IF (J .EQ. KCLD1) D10 = TIME1(N3)                                         
      IF (J .EQ. KCLQ1) D11 = TIME1(N3)                                         
C  REVERSE EXCITATION VOLTAGE POLARITY DUE TO UM SIGN CONVENTION        M42.5546
      IF (NODE(N3) .LT. 0) GO TO 2090                                           
      IF (LFIM4I .EQ. 0) GO TO 2090                                     M42.5548
      CREST(N3) = - CREST(N3)/SLIP                                      M42.5549
C  NEGATIVE SEQUENCE CONVERSION TO POSITIVE IF SLIP .LT. 0.0 :          M42.5550
      IF (SLIP .LT. 0.0) TIME1(N3) = - TIME1(N3)                        M42.5551
 2090 IF (IPRSUP .GE. 1)                                                M42.5552
     1 WRITE (LUNIT6,2088) NODE(N3),N3,SFREQ(N3),CREST(N3)              M42.5553
 2088 FORMAT(10H *********,                                             M42.5554
     1       31H ADJUSTED IM EXCITATION SOURCES,3X,I4,50X,              M42.5555
     2         I6,E14.5,1X,E13.5)                                       M42.5556
 2089 N3 = KPSOUR(N3)                                                   M42.5557
      IF (N3 .EQ. N4) GO TO 2094                                        M42.5558
      SFREQ(N3) = D1                                                    M42.5559
      IF (N3 .GE. N19 .AND. N3 .LE. N20) GO TO 2093                     M42.5560
      N18 = N18 + 1                                                     M42.5561
      IF (J .EQ. KCLD1) D10 = TIME1(N3)                                         
      IF (J .EQ. KCLQ1) D11 = TIME1(N3)                                         
      IF (NODE(N3) .LT. 0) GO TO 2089                                           
      IF (LFIM4I .EQ. 0) GO TO 2089                                     M42.5565
      CREST(N3) = - CREST(N3)/SLIP                                      M42.5566
      IF (SLIP .LT. 0.0) TIME1(N3) = - TIME1(N3)                        M42.5567
      IF (NMINUM .GT. N3) NMINUM = N3                                   M42.5568
 2093 GO TO 2090                                                        M42.5569
 2094 CONTINUE                                                          M42.5570
C  CHECK FOR CORRECT PHASE SEQUENCE OF EXTERNAL EXC SOURCES :           M42.5571
      IF (IMACH .NE. 1) GO TO 2097                                      M42.5572
      IF (N18 .EQ. 0) GO TO 2097                                                
      D12 = D10 - D11                                                           
      D13 = TWOPI/36.0                                                          
      D14 = TWOPI/3.0                                                   M42.5576
      IF (JTYPE(K) .EQ. 3) D14 = - TWOPI/4.0                            M42.5577
      D15 = 2.0 * D14                                                   M42.5578
      IF (JTYPE(K) .EQ. 3) D15 = - D14                                  M42.5579
      IF (SLIP .GT. 0.0) GO TO 2096                                     M42.5580
      D16 = - D14 - D13                                                 M42.5581
      D17 = - D14 + D13                                                 M42.5582
      IF (D12 .GT. D16 .AND. D12 .LT. D17) GO TO 2097                           
      D16 = D15 - D13                                                   M42.5584
      D17 = D15 + D13                                                   M42.5585
      IF (D12 .GT. D16 .AND. D12 .LT. D17) GO TO 2097                           
      WRITE (LUNIT6,2095) K                                             M42.5587
 2095 FORMAT(/,22H ERROR STOP. UM NUMBER,I4,16H IS AN IND. MACH,        M42.5588
     1         42H AND IS REQUESTED TO BE INITIALIZED WITH A,           M42.5589
     2         43H NEGATIVE SLIP VALUE. THE PHASE SEQUENCE OF           M42.5590
     3  ,/,    45H THE SPECIFIED EXTERNAL EXCITATION SOURCES IS,        M42.5591
     4         42H INCORRECT. IT SHOULD BE NEGATIVE SEQUENCE,           M42.5592
     5         31H RATHER THAN POSITIVE SEQUENCE.)                      M42.5593
      CALL STOPTP                                                       M42.5594
 2096 D16 = D14 - D13                                                   M42.5595
      D17 = D14 + D13                                                   M42.5596
      IF (D12 .GT. D16 .AND. D12 .LT. D17) GO TO 2097                           
      D16 = - D15 - D13                                                 M42.5598
      D17 = - D15 + D13                                                 M42.5599
      IF (D12 .GT. D16 .AND. D12 .LT. D17) GO TO 2097                           
      WRITE (LUNIT6,82096) K                                            M42.5601
82096 FORMAT(/,22H ERROR STOP. UM NUMBER,I4,16H IS AN IND. MACH,        M42.5602
     1         42H AND IS REQUESTED TO BE INITIALIZED WITH A,           M42.5603
     2         43H POSITIVE SLIP VALUE. THE PHASE SEQUENCE OF           M42.5604
     3  ,/,    45H THE SPECIFIED EXTERNAL EXCITATION SOURCES IS,        M42.5605
     4         42H INCORRECT. IT SHOULD BE POSITIVE SEQUENCE,           M42.5606
     5         31H RATHER THAN NEGATIVE SEQUENCE.)                      M42.5607
      CALL STOPTP                                                       M42.5608
 2097 IF (NMINUM .GE. 10000) GO TO 2100                                 M42.5609
      IF (NODE(NMINUM) .GT. 0) GO TO 2100                               M42.5610
      IF (LFIM4I .EQ. 0) GO TO 2100                                     M42.5611
      WRITE(LUNIT6,2098)                                                M42.5612
 2098 FORMAT(/,43H WARNING :   YOU HAVE REQUESTED A LOAD-FLOW,          M42.5613
     1         38H FOR A NETWORK CONTAINING AN INDUCTION,               M42.5614
     2         42H MACHINE. YOU CHOSE THE EXCITER SOURCES AS            M42.5615
     3 ,/,     42H CURRENT SOURCES AND WILL THEREFORE NOT BE,           M42.5616
     4         34H ADJUSTED BY THE LOAD-FLOW MODULE.,/)                 M42.5617
 2100 IF (JTMTAC(K) .LE. 0) GO TO 2150                                  M42.5618
      WRITE (LUNIT6,2110)                                               M42.5619
 2110 FORMAT(24H0PROGRAM STOP BECAUSE OF,                               M42.5620
     1       26H CONFLICTING REQUEST, I.E.)                             M42.5621
      WRITE (LUNIT6,2120)                                               M42.5622
 2120 FORMAT(35H NO INITIALIZATION IF NO MECHANICAL,                    M42.5623
     1       15H NETWORK OPTION)                                        M42.5624
      CALL STOPTP                                                               
C   CREATING TEMPORARY SPEED-SOURCE FOR EACH UM MASS :                  M42.5626
 2150 IF (LOOPSS(10) .EQ. 3) GO TO 2200                                 M42.5627
      KCONST = KCONST + 1                                                       
      IF ( KCONST .GE. LEXCT )  GO TO 6788                              M42.5629
      IFORM(KCONST) = 14                                                M42.5630
      NODE(KCONST) = NODOM(K)                                           M42.5631
      N1 = JCLTAC(KCL)                                                          
      N2 = JCLTAC(KCL+1)                                                        
      IF (N1 .EQ. 0 .AND. N2 .EQ. 0) GO TO 2154                         M42.5634
      N4 = K                                                                    
      IF (N1 .NE. 0 .AND. K .GT. N1) N4 = N1                            M42.5636
      IF (N2 .NE. 0 .AND. N4 .GT. N2) N4 = N2                           M42.5637
      IF (K .EQ. N4) GO TO 2154                                                 
      NODE(KCONST) = - JTMTAC(K)                                                
 2154 TSTART(KCONST) = -1.0                                             M42.5640
      TSTOP(KCONST) = 0.0                                               M42.5641
      N10 = NODMUM(K)                                                   M42.5642
      SFREQ(KCONST) = SFREQ(N10)                                        M42.5643
      CREST(KCONST) = 0.0                                                       
 2160 FORMAT(10H *********,                                             M42.5645
     1       21H UM MASS SPEED-SOURCE,13X,I4,50X,                       M42.5646
     2         I6,E14.5,1X,E13.5)                                       M42.5647
      IF (IPRSUP .GE. 1)                                                M42.5648
     1 WRITE(LUNIT6,2160) NODE(KCONST),KCONST,SFREQ(KCONST),            M42.5649
     2                    CREST(KCONST)                                 M42.5650
 2200 CONTINUE                                                          M42.5651
C  LOAD-FLOW PREPARATION OF IM TYPE-4 TEMPORARY BRANCHES, WHICH         M42.5652
C   ARE TO BE WIPED OUT IN THE SECOND PART OF THE UMRENU CODE :         M42.5653
      IF (LOOPSS(10) .NE. 1) GO TO 2250                                 M42.5654
      DO 2248 K = 1,NUMUM                                               M42.5655
      IF (JTYPE(K) .LT. 3) GO TO 2248                                   M42.5656
      IF (JTYPE(K) .GT. 4) GO TO 2248                                   M42.5657
      SLIP = VOLTUM(K)/100.0                                            M42.5658
      IF (IPRSUP .GE. 1) WRITE(LUNIT6,2201)                             M42.5659
 2201 FORMAT(/,40H ***************************************,             M42.5660
     1         43H MOMENTARY CHANGES OR CREATION OF MOMENTARY,          M42.5661
     2         44H ELEMENTS FOR LOAD-FLOW INVOLVING IND. MACH.)         M42.5662
      IF (IPRSUP .GE. 1) WRITE(LUNIT6,1904) K                           M42.5663
      KCL = KCOIL(K)                                                    M42.5664
      NCL = NCLD(K) + NCLQ(K)                                                   
      IF (JTYPE(K) .EQ. 3) GO TO 2226                                   M42.5666
      IF (JTYPE(K) .EQ. 4) NCL = NCL + 1                                M42.5667
C  SET TEMPORARY IM ROTOR BRANCHES FOR LD-FLOW :                        M42.5668
      NDUM(31) = NODVO1(KCL+5)                                          M42.5669
      NDUM(32) = NODVO1(KCL+3)                                          M42.5670
      NDUM(33) = NODVO1(KCL+4)                                          M42.5671
      NDUM(34) = NODVO2(KCL+5)                                          M42.5672
      NDUM(35) = NODVO2(KCL+3)                                          M42.5673
      NDUM(36) = NODVO2(KCL+4)                                          M42.5674
      DO 2220 N1 = 31,33                                                M42.5675
      N3 = NDUM(N1)                                                     M42.5676
      N4 = NDUM(N1+3)                                                   M42.5677
 2202 CALL IBRINC                                                       VAX.  73
      IT = IT + 1                                                       M42.5679
      N5 = KCL + N1 - 31                                                M42.5680
      KBUS(IBR) = NODVO1(N5)                                            M42.5681
      IF (NODVO1(N5) .EQ. 1) KBUS(IBR) = NODVO2(N5)                     M42.5682
      MBUS(IBR) = N3                                                    M42.5683
      IF (N4 .NE. 1) MBUS(IBR) = N4                                     M42.5684
      LENGTH(IBR) = 1                                                   M42.5685
      NR(IBR) = - IT                                                    M42.5686
      TR(IT) = 1.0/(GPAR(KCL+4) * SLIP)                                         
      C(IT) = 0.0                                                       M42.5688
      TX(IT) = REACL(KCL+4) * 1.0D+3                                    M42.5689
      IF (XOPT .NE. 0.0) TX(IT) = REACL(KCL+4) * TWOPI * XOPT           M42.5690
      IF (IPRSUP .GE. 1)                                                M42.5691
     1 WRITE(LUNIT6,2208) KBUS(IBR),MBUS(IBR),IBR,IT,                   M42.5692
     2                    TR(IT),TX(IT)                                 M42.5693
 2208 FORMAT(10H *********,                                             M42.5694
     1       34H IM ROTOR BRANCHE JUST FOR LD-FLOW,                     M42.5695
     2        I4,4X,I4,2I6,2E14.5)                                      M42.5696
 2220 CONTINUE                                                          M42.5697
      IF (NDUM(33) .NE. 1 .AND. NDUM(36) .NE. 1) GO TO 2222             M42.5698
      IF (NODVO1(KCL) .EQ. 1 .OR. NODVO2(KCL) .EQ. 1) GO TO 2226        M42.5699
 2222 WRITE(LUNIT6,2224)                                                M42.5700
 2224 FORMAT(/,43H ERROR STOP. YOU HAVE REQUESTED A LOAD-FLOW,          M42.5701
     1         45H OF A NETWORK CONTAINING AN INDUCTION MACHINE,        M42.5702
     2         35H WITH UNGROUNDED POWER COILS AND/OR                   M42.5703
     3  ,/,    38H EXCITATION COILS. THIS REQUEST IS NOT,               M42.5704
     4         43H HONORED IN THE CURRENT EMTP VERSION. THESE,          M42.5705
     5         38H COILS ARE BOTH TO BE Y-CONNECTED WITH,               M42.5706
     6         19H GROUNDED NEUTRALS.)                                  M42.5707
      CALL STOPTP                                                               
C  SET TEMPORARY BRANCHES FOR IM MAIN INDUCTANCES IF FULL COMP :        M42.5709
 2226 IF (LOOPSS(8) .NE. 0) GO TO 2230                                  M42.5710
      DO 2228 N1 = 1,3                                                  M42.5711
      N2 = N1 + KCL - 1                                                 M42.5712
      CALL IBRINC                                                       VAX.  74
      IT = IT + 1                                                       M42.5714
      IF (N1 .EQ. 3) HIST(KCL) = IBR                                    M42.5715
      KBUS(IBR) = NODVO1(N2)                                            M42.5716
      MBUS(IBR) = NODVO2(N2)                                            M42.5717
      LENGTH(IBR) = 1                                                   M42.5718
      NR(IBR) = - IT                                                    M42.5719
      TR(IT) = 0.0                                                      M42.5720
      C(IT) = 0.0                                                       M42.5721
      IF (JTYPE(K) .NE. 3) GO TO 2227                                   M42.5722
      TR(IT) = EPSILN                                                   M42.5723
      TX(IT) = 0.0                                                      M42.5724
      IF (IPRSUP .GE. 1)                                                M42.5725
     1 WRITE(LUNIT6,2054) KBUS(IBR),MBUS(IBR),IBR,IT,TR(IT),            M42.5726
     2 TX(I)                                                            M42.5727
      GO TO 2228                                                        M42.5728
 2227 TX(IT) = REAMQU(K) * 1.0D+3                                       M42.5729
      IF (XOPT .NE. 0.0) TX(IT) = REAMQU(K) * TWOPI * XOPT              M42.5730
      IF (IPRSUP .GE. 1)                                                M42.5731
     1 WRITE(LUNIT6,2052) KBUS(IBR),MBUS(IBR),IBR,IT,TR(IT),            M42.5732
     2 TX(I)                                                            M42.5733
 2228 CONTINUE                                                          M42.5734
      IF (JTYPE(K) .EQ. 3) GO TO 2250                                   M42.5735
C  ADJUSTING EXTERNAL EXCITATION NETWORK RESISTANCES OF IM INTO         M42.5736
C    STATOR FREQUENCY DOMAIN IN CASE OF LOAD-FLOW REQUEST :             M42.5737
 2230 DO 2232 N1 = 1,40                                                         
 2232 NDUM(N1) = 0                                                      M42.5739
      N1 = 0                                                                    
      DO 2242 N13 = 1,NCL                                                       
      N14 = KCL + 2 + N13                                               M42.5742
      N2 = NODVO1(N14)                                                          
      IF (N2 .EQ. 1) N2 = NODVO2(N14)                                   M42.5744
      IF (N2 .EQ. 1) GO TO 2248                                                 
      N3 = KSSFRQ(N2)                                                           
      N4 = N3                                                           M42.5747
 2240 N1 = N1 + 1                                                               
      NDUM(N1) = N3                                                             
      N3 = KPSOUR(N3)                                                           
      IF (N3 .NE. N4) GO TO 2240                                                
 2242 CONTINUE                                                          M42.5752
      N13 = LOOPSS(5)                                                           
      VINP(1) = 0.0                                                     M42.5754
      N18 = 1                                                                   
      IF (N1 .LT. 40) GO TO 82242                                       M42.5756
      WRITE(LUNIT6,82245) K                                             M42.5757
      CALL STOPTP                                                       M42.5758
82242 DO 2245 N10 = 1,N13                                                       
      N11 = KBUS(N10)                                                           
      IF (N11 .LT. 0) N11 = - N11                                       M42.5761
      N12 = MBUS(N10)                                                   M42.5762
      IF (N12 .LT. 0) N12 = - N12                                       M42.5763
      N3 = KSSFRQ(N11)                                                          
C   NOW DETERMINE BRANCHES IN EXC SUBNETWORK WITH SOURCES NDUM          M42.5765
      DO 2244 N8 = 1,N1                                                         
      IF (N3 .NE. NDUM(N8)) GO TO 2244                                          
      N9 = NR(N10)                                                              
      IF (N9 .LT. 0) N9 = - N9                                          M42.5769
C   AVOID REINITIALIZATION OF REFERENCED EXCIT BRANCHES :               M42.5770
      N17 = 0                                                           M42.5771
      DO 82243 N19 = 1,N18                                                      
      N20 = VINP(N19)                                                   M42.5773
      IF (N9 .EQ. N20) N17 = 1                                          M42.5774
82243 CONTINUE                                                          M42.5775
      IF (N17 .EQ. 1) GO TO 82244                                               
      N18 = N18 + 1                                                     M42.5777
      VINP(N18) = N9                                                    M42.5778
      TR(N9) = TR(N9)/SLIP                                              M42.5779
82244 IF (N18 .LT. 40) GO TO 82246                                      M42.5780
      WRITE(LUNIT6,82245) K                                             M42.5781
82245 FORMAT(/,22H ERROR STOP. UM NUMBER,I4,14H HAS MORE THAN,          M42.5782
     1         44H 40 EXCITATION BRANCHES OR SOURCES. OVERFLOW,         M42.5783
     2         42H IS AVOIDED BY INCREASING THE DIMENSION OF            M42.5784
     3   ,/,   45H THE ARRAYS "VINP" AND "NDUM" IN UMDECK TO BE,        M42.5785
     4         37H ONE HIGHER THAN THESE USED ELEMENTS.)                M42.5786
      CALL STOPTP                                                       M42.5787
82246 IF (IPRSUP .GE. 1)                                                M42.5788
     1 WRITE(LUNIT6,2243) N11,N12,N10,N9,TR(N9),TX(N9)                  M42.5789
 2243 FORMAT(10H *********,                                             M42.5790
     1       34H CHANGE EXT EXC R JUST FOR LD-FLOW,                     M42.5791
     2         I4,4X,I4,2I6,2E14.5)                                     M42.5792
      GO TO 2245                                                                
 2244 CONTINUE                                                          M42.5794
 2245 CONTINUE                                                          M42.5795
 2248 CONTINUE                                                          M42.5796
C  LD-FLOW CONDITIONS IF LD-FLOW NOT YET CONDUCTED ******** :           M42.5797
C  (A) CONDITION OF FIRST UMRENU PASS, UNLESS UM TYPE-3 IS PRESE        M42.5798
C      WHICH WOULD MAKE IT THE SECOND PASS.                             M42.5799
 2250 IF (LOOPSS(10) .NE. 1) GO TO 2260                                 M42.5800
      LOOPSS(10) = 2                                                            
      IF (LFIM3 .EQ. 1) RETURN                                                  
      GO TO 2400                                                                
C  (B) CONDITION OF FIRST PASS THROUGH "FIRST PART" IF UM TYPE-3        M42.5804
C      IS PRESENT.                                                      M42.5805
 2260 IF (LOOPSS(10) .NE. 3) GO TO 2280                                         
      ISTEP = 0                                                                 
 2280 IF (NCOMUM .EQ. 0) KSUBUM = KSUBUM - NUMSUB - 1                   M42.5808
      IF (NCOMUM .EQ. 0) GO TO 2300                                     M42.5809
      LOOPSS(2) = 3 * NTOT                                              M42.5810
      ISUBEG(NUMSUB+1) = N15 + 1                                        M42.5811
 2300 IF (LOOPSS(10) .EQ. 3) GO TO 2400                                 M42.5812
      IF (LOOPSS(10) .EQ. 2) LOOPSS(10) = 0                                     
C  DETECTING PRESENCE OF ZERO COMPONENT CURRENT :                       M42.5814
      IF (LOOPSS(8) .EQ. 0) GO TO 2400                                  M42.5815
      DO 2350 N5 = 1,NUMUM                                              M42.5816
      N1 = - 9999                                                       M42.5817
      N3 = KCOIL(N5)                                                    M42.5818
      IF (NODVO2(N3+2) .EQ. 1) N1 = 0                                   M42.5819
      IF (NODVO1(N3+2) .EQ. 1) N1 = 0                                   M42.5820
      IF (N1 .EQ. 0) GO TO 2350                                         M42.5821
      DO 2310 N2 = 1,IBR                                                M42.5822
      IF (KBUS(N2) .NE. NODVO2(N3+2)) GO TO 2310                        M42.5823
      IF (MBUS(N2) .EQ. NODVO1(N3+2)) GO TO 2310                        M42.5824
      N1 = MBUS(N2)                                                     M42.5825
 2310 CONTINUE                                                          M42.5826
      IF (N1 .EQ. -9999) GPAR(N3) = 0.0                                 M42.5827
      IF (N1 .EQ. -9999) REACL(N3) = 0.0                                M42.5828
 2350 CONTINUE                                                          M42.5829
 2400 RETURN                                                            M42.5830
      END                                                               M42.5831
      SUBROUTINE EQUIV(REQ,XEQ,GEQ,BEQ,Z,R,TAU,W,N)                     M31.2425
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      DIMENSION REQ(1), XEQ(1), GEQ(1), BEQ(1), Z(1), R(1), TAU(1)      M05. 433
      EQUIVALENCE    (ANUMR, RZERO),    (ANUMX, XZERO),                 M42.5832
     1 (YPOS, SINB),     (YNEG, COSB),     (EPOS, SINHGR),              M42.5833
     2 (ENEG, SINHGI),     (C, COSHA),     (D, SINHA)                   M42.5834
      DO 5 L=1,N                                                        M42.5835
      J=L                                                               M42.5836
      A = W * TAU(J)                                                    M05. 434
      ZS=Z(J)                                                           M42.5837
      RS=R(J)                                                           M42.5838
      IF ( IPRSUP  .GE.  3 )                                            M22.3239
     1 WRITE (LUNIT6, 1786)  L, N, W, TAU(J), ZS, RS                    M22.3240
 1786 FORMAT ( /,  28H WITHIN L-LOOP OF  'EQUIV' .,                     M22.3241
     1 16H       L       N,  15X,  1HW,  10X,  6HTAU(J),                M22.3242
     2 14X,  2HZS,  14X,  2HRS  ,/,  28X,  2I8,  4E16.7  )              M22.3243
      IF( RS .GE. 0.0 )  GO TO 1                                        M42.5839
      RS = -RS                                                          M42.5840
      DENR = A * ZS                                                     M42.5841
      DENX = A / ZS                                                     M42.5842
      PIOV2 = TWOPI * 0.25                                              M05. 435
      D1 = SQRTZ ( RS**2  +  DENR**2 )                                  M18. 417
      D2 = ATAN2Z ( DENR, RS )                                          M18. 418
      D3 = 0.5 * ( D2 - PIOV2 )                                         M05. 438
      D4 = SQRTZ ( D1 / DENX )                                          M18. 419
      RZERO = D4 * COSZ(D3 )                                            M18. 420
      XZERO = D4 * SINZ( D3 )                                           M18. 421
      D3 = D3 + PIOV2                                                   M05. 442
      D4 = SQRTZ ( D1 * DENX )                                          M18. 422
      E = D4 * COSZ(D3)                                                 M18. 423
      A = D4 * SINZ(D3)                                                 M18. 424
      YPOS = EXPZ(E)                                                    M18. 425
      YNEG = 1.0 / YPOS                                                 M42.5843
      COSHA = ( YPOS + YNEG ) / 2.0                                     M05. 446
      SINHA = COSHA - YNEG                                              M42.5844
      SINB = SINZ(A)                                                    M18. 426
      COSB = COSZ(A)                                                    M18. 427
      SINHGR = SINHA * COSB                                             M42.5845
      SINHGI = COSHA * SINB                                             M42.5846
      DENR = RZERO * SINHGR  -  XZERO * SINHGI                          M42.5847
      DENX = RZERO * SINHGI  +  XZERO * SINHGR                          M42.5848
      ANUMR = COSHA * COSB  -  1.0                                      M42.5849
      ANUMX = SINHA * SINB                                              M42.5850
      GO TO 2                                                           M42.5851
    1 E = RS / ZS                                                       M42.5852
      IF(ZS.LT.0.) GO TO 3                                              M42.5853
      A=A / 2.0                                                         M05. 447
      C=SINZ(A)                                                         M18. 428
      D=COSZ(A)                                                         M18. 429
      E2=E**2                                                           M42.5854
      C2=C**2                                                           M42.5855
      DENR  =  ( 48. + E2 ) / 32.                                       M05. 448
      DENR  =  ( 1.0  - C2 * DENR ) * RS                                M05. 449
      E2DIV8 = E2 / 8.0                                                 M05. 450
      DENX=( 3.0*E2DIV8 +2.0)*C*D*ZS                                    M05. 451
      ANUMR=(-2.0- E2DIV8 )*C2                                          M05. 452
      ANUMX=E*C*D                                                       M42.5856
    2 E = DENR**2  +  DENX**2                                           M42.5857
      REQ(L)=DENR                                                       M42.5858
      XEQ(L)=DENX                                                       M42.5859
    4 GEQ(L)=(ANUMR*DENR+ANUMX*DENX)/E                                  M42.5860
      BEQ(L)=(ANUMX*DENR-ANUMR*DENX)/E                                  M42.5861
      GO TO 5                                                           M42.5862
    3 E=E / 2.0                                                         M05. 453
      C=SINZ(A)                                                         M18. 430
      D=COSZ(A)                                                         M18. 431
      YPOS=EXPZ(-E)                                                     M18. 432
      YNEG=1.0/YPOS                                                     M42.5863
      EPOS=YPOS+YNEG                                                    M42.5864
      ENEG=YPOS-YNEG                                                    M42.5865
      ANUMR=D*EPOS-2.                                                   M42.5866
      ANUMX=C*ENEG                                                      M42.5867
      DENR=D*ENEG                                                       M42.5868
      DENX=C*EPOS                                                       M42.5869
      TDEN=(DENR*DENR+DENX*DENX)                                        M42.5870
      E = - ZS / 2.0                                                    M05. 454
      REQ(L) = E * DENR                                                 M42.5871
      XEQ(L) = E * DENX                                                 M42.5872
      E=-ZS*TDEN                                                        M42.5873
      GO TO 4                                                           M42.5874
    5 CONTINUE                                                          M42.5875
      RETURN                                                            M31.2426
      END                                                               M42.5876
 
