      SUBROUTINE SUBR39                                                 M32.7612
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL39.FOR;/NOLIST'                                           
      INCLUDE  'DECK39.FOR;/NOLIST'                                           
      DIMENSION  LLTEMP(20)                                             M32.7616
      EQUIVALENCE ( KDEFLT, INDTV(1) )                                          
      REAL*8         TEXT1, TEXT2, TEXT3, TEXT4, TEXT5, TEXT6           M32.7619
      REAL*8         TEXT7, TEXT8, TEXT9, TEXT10, TEXT11, TEXT12        M32.7620
      REAL*8         TEXTA, TEXTP, TEXT13, TEXT14, TEXT15               M42.7939
      DIMENSION  TEXTA(14), TEXTP(14)                                   M32.7622
      DIMENSION  ALINTP(4100)                                                   
      DIMENSION  MINUST(11)                                                     
      DIMENSION  AKFRS1(100), AKFRS2(100), ALPHS1(100), ALPHS2(100)     M43.2894
      DATA  TEXT1   /  6HLINE    /                                      M32.7623
      DATA  TEXT2   /  6HCONSTA  /                                      M32.7624
      DATA  TEXT3   /  6HNTS     /                                      M32.7625
      DATA  TEXT4   /  6HCABLE   /                                      M32.7626
      DATA  TEXT5   /  6HLC      /                                      M32.7627
      DATA  TEXT6   /  6HCC      /                                      M32.7628
      DATA  TEXT7   /  6HOLD    /                                       M32.7629
      DATA  TEXT8   /  6HDATA   /                                       M32.7630
      DATA  TEXT9   /  6HNEW    /                                       M32.7631
      DATA  TEXT10  /  6HRHO    /                                       M32.7632
      DATA  TEXT11  /  6HBRANCH  /                                      M32.7633
      DATA  TEXT12  /  6HCOMMON  /                                      M32.7634
      DATA  TEXT13  /  6HCARDS  /                                       M35.8876
      DATA  TEXT14  /  6HPRINTE /                                       M42.7940
      DATA  TEXT15  /  6HR PLOT /                                       M42.7941
      IF ( IPRSUP  .GE.  1 )                                            M32.7637
     1 WRITE (LUNIT6, 1546)  LASTOV                                     M32.7638
 1546 FORMAT (  27H TOP OF "SUBR39".  LASTOV =,  I6 )                   M32.7639
      IF (LASTOV .EQ. 1 )  GO TO 5657                                   M43.2895
      ONEHAV = ONEHAF                                                   M32.7640
      ONEQTR = 0.25D0                                                   M32.7641
      IFQDPT = 1                                                                
      PAI = TWOPI/2.                                                    M43.2898
      TENDEG = PAI / 18.                                                M43.2899
C ==== CHANGE THESE LIMITS IF DIMENSIONS IN LABL39 ARE CHANGED:         M32.7642
C   NUMBER OF MODES:                                                    M32.7643
      MMODES = 18                                                       M32.7645
C   CURVE ZONES = PEAKS+VALLEYS+1 :                                     M32.7646
C   XKNEE(MXKNEE),NOPRAO(MXKNEE)                                        M32.7647
C     THL + Edwin + WSM, 1 Aug 1986.  Change "LABL39" arrays 20 to 100: M43.2900
      MXKNEE = 100                                                      M35.8877
C   ORDER OF THE APPROXIMATION = NO. OF POLES :                         M32.7649
C   XAUXD(2*MPOLES),FCZ(MPOLES),FCP(MPOLES),                            M32.7650
C   FCZR(MPOLES),FCPR(MPOLES),INDXV(MPOLES),                            M32.7651
C   AKFRAC(MPOLES),ALPHAF(MPOLES)                                       M32.7652
      MPOLES = 100                                                      M32.7653
C   DIMENSION OF XCHKRA = 2*SUM(2**I), I=0,N ; N SUCH THAT 2**N.GE.MPOLEM32.7654
C   XCHKRA(MXCHKR)                                                      M32.7655
      MXCHKR = 255                                                      M32.7656
C   DIMENSION OF ZOPRAU, ZOPRAO, AZEPO = 4*MPOLES  :                    M32.7657
C   ZOPRAU(MZOPRA),ZOPRAO(MZOPRA),AZEPO(MZOPRA)                         M32.7658
      MZOPRA = 400                                                      M32.7659
C ====  END OF DIMENSION-LIMIT ASSIGNMENTS ("LABL39" STORAGE)           M32.7660
      LUNT35 = 35                                                       M32.7661
      LLM1 = -1                                                         M32.7662
      LL4 = 4                                                           M35.8878
      JDATCS = 0                                                        M35.8879
      CALL DIMENS ( LLTEMP(1), NCHAIN, TRASH, TRASH )                   M32.7663
      IF ( IPRSUP .GE. 1)                                               M33.2182
     1  WRITE (6, 7001)   LLTEMP(1), LLTEMP(2)                                 
 7001 FORMAT ( /, 1X, 13HLLTEMP(1:2) =,  2I8  )                        
      N3 = 0                                                            M35.8880
      DO 5654  I=1, 9999, 2                                             M35.8881
      IF ( N3  .GE.  1 )   GO TO 1550                                   M35.8882
      IF ( LLTEMP(I)  .NE.  71 )   GO TO 5654                           M35.8883
      MDAPTS = LLTEMP(I+1)                                              M35.8884
      N3 = N3 + 1                                                       M35.8885
 5654 CONTINUE                                                          M35.8886
      CALL STOPTP                                                               
 1550 N13 = 0                                                           M32.7667
      KDEFLT = 0                                                        M32.7668
      IF(LASTOV.NE.1) GO TO 4040                                        M32.7669
 5657 IALDUM = 0                                                        M32.7670
      IALTER = 1                                                        M32.7671
      DO  7004 I=1, 24                                                 
 7004 VSTACS(I) = BLANK                                                 
      SGLFIR = 0.2D0                                                    M42.7942
      M = 1                                                             M32.7674
C     READ INPUT CARD USING CIMAGE                                      M32.7675
 4040 CALL CIMAGE                                                       M32.7676
      NRIGHT = -2                                                       M32.7677
      N4 = KOLBEG                                                       M32.7678
      KOLBEG = 1                                                        M32.7679
      CALL FREONE ( D1 )                                                M32.7680
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M32.7681
      NRIGHT = 0                                                        M32.7682
      IF ( TEXTA6(1)  .NE.  TEXT11 )   GO TO 7618                       M37.7979
C     OPTIONAL "BRANCH" CARD GIVING PAIRS OF NODE NAMES                 M32.7697
      N1 = M + 11                                                       M32.7698
      DECODE (80,7609,ABUFF) (VSTACS(I), I=M, N1)                       M32.7699
 7609 FORMAT(8X,12A6)                                                   M32.7700
      IF (IPRSUP .GE. 6 )                                               M32.7701
     1 WRITE (LUNIT6, 7613)  M, (VSTACS(I), I=M, N1)                    M32.7702
 7613 FORMAT ( 14H VSTACS NAMES:,  12A6  )                              M32.7703
      M = M + 12                                                        M32.7704
      WRITE (KUNIT6, 7616)                                              M35.8888
 7616 FORMAT (26H+BUS NAMES FOR EACH PHASE.)                            M32.7706
      GO TO 4040                                                        M32.7707
 7618 IF(TEXTA6(1).NE.TEXT7 ) GO TO 4042                                M32.7708
      IF(TEXTA6(2).NE.TEXT8 ) GO TO 4042                                M32.7709
      IALDUM = 3                                                        M32.7710
      IALTER = 3                                                        M32.7711
      IF ( KOLBEG  .GT.  0 )   GO TO 7623                               M32.7712
      DECODE (80, 7622, ABUFF(1))  DIST                                 M32.7713
 7622 FORMAT ( 24X,  E8.0 )                                             M32.7714
      GO TO 7624                                                        M32.7715
 7623 NFRFLD = 1                                                        M32.7716
      CALL FREONE ( DIST )                                              M32.7717
 7624 IF ( METRIK  .EQ.  0   .AND.  N13  .NE.  8765 )                   M32.7718
     1  DIST = DIST * 1.60935                                           M32.7719
      WRITE (KUNIT6, 7625)  DIST                                        M35.8889
 7625 FORMAT ( 33H+SAME LINE, BUT WITH NEW LENGTH =,                    M37.7980
     1  F9.2,   5H  KM.   )                                             M32.7722
      GO TO 4040                                                        M32.7723
 4042 IF(TEXTA6(1).NE.TEXT9) GO TO 4043                                 M32.7724
      IF(TEXTA6(2).NE.TEXT10) GO TO 4043                                M32.7725
C     TEMPORARY NEW RHO AREA - TO BE COMPLETED LATER, SOMEDAY           M32.7726
 4043 IF ( TEXTA6(1) .NE. TEXT14 )  GO TO 8819                          M42.7943
      IF ( TEXTA6(2) .NE. TEXT15 )  GO TO 8819                          M42.7944
      DECODE (80, 7622, ABUFF) SGLFIR                                   M42.7945
      WRITE (KUNIT6, 8814)  SGLFIR                                      M42.7946
 8814 FORMAT ( 42H+NEW LOG-F/LINE OF PRINTER PLOT.  SGLFIR =, F8.4 )    M42.7947
 8819 CONTINUE                                                          M42.7948
      DECODE (80, 1019, ABUFF(1))  ( TEXTA(K), K=1, 14 )                M32.7728
 1019 FORMAT ( 13A6, A2 )                                               M32.7729
      DO 1023  K=1, 14                                                  M32.7730
      IF ( TEXTA(K)  .NE.  BLANK )  GO TO 4044                          M32.7731
 1023 CONTINUE                                                          M32.7732
      WRITE (KUNIT6, 1719)                                              M35.8890
 1719 FORMAT(30H+BLANK CARD ENDING MARTI SETUP )                        M32.7734
      CALL INTERP                                                       M35.8891
      GO TO 9200                                                        M32.7740
C     CHECK FOR KEY WORD  'LINE CONSTANTS'  (OR  'LC' ).                M32.7741
 4044 IF ( TEXTA6(1)  .EQ.  TEXT5 )   GO TO 1706                        M32.7742
      IF ( TEXTA6(1)  .NE.  TEXT1 )   GO TO 4049                        M32.7743
      IF ( TEXTA6(2)  .NE.  TEXT2 )   GO TO 4049                        M32.7744
      IF ( TEXTA6(3)  .NE.  TEXT3 )   GO TO 4049                        M32.7745
 1706 IF ( IPRSUP .GE. 1 )                                              M35.8892
     1 WRITE (LUNIT6, 4045)                                             M35.8893
 4045 FORMAT (  32H TRANSFER TO  'LINE CONSTANTS' .     )               M35.8894
      LASTOV = NCHAIN                                                   M32.7748
      NCHAIN = 44                                                       M32.7749
      GO TO 9999                                                        M32.7750
C     CHECK FOR KEY WORD  'CABLE CONSTANTS'  (OR  'CC' ).               M32.7751
 4049 IF ( TEXTA6(1)  .EQ.  TEXT6 )   GO TO 1716                        M32.7752
      IF ( TEXTA6(1)  .NE.  TEXT4 )   GO TO 4055                        M32.7753
      IF ( TEXTA6(2)  .NE.  TEXT2 )   GO TO 4055                        M32.7754
      IF ( TEXTA6(3)  .NE.  TEXT3 )   GO TO 4055                        M32.7755
 1716 WRITE (KUNIT6, 4053)                                              M35.8895
 4053 FORMAT (  33H+TRANSFER TO  'CABLE CONSTANTS' .     )              M32.7757
      LASTOV = NCHAIN                                                   M32.7758
      NCHAIN = 47                                                       M32.7759
      GO TO 9999                                                        M32.7760
 4055 IF( TEXTA6(1) .NE. TEXT8 )   GO   TO  4057                        M35.8896
      IF( TEXTA6(2) .NE. TEXT13 )   GO   TO  4057                       M35.8897
C     READ DATA CARD USING CIMAGE                                       M35.8898
      CALL CIMAGE                                                       M35.8899
      DECODE(80,4027,ABUFF(1)) KTAB,IMODAL,METRIK,D9                    M39.3310
 4027 FORMAT(3I8,E8.0)                                                  M35.8901
      CK1 = D9                                                          M35.8902
      IF( METRIK .EQ. 0 )  CK1 = CK1 * .62135896D0                      M35.8903
CCC      write (*,*) ' subr39.  Ready to to rewind lunit9 =',           M43.2901
CCC     1                                          lunit9               M43.2902
      REWIND LUNIT9                                                     M35.8904
C     WRITE FIVE DUMMY ZEROS                                            M43.2903
      D11 = 0.                                                          M35.8906
      WRITE( LUNIT9 ) D11, D11, D11, D11,D11                            M43.2904
      REWIND LUNIT9                                                     M35.8908
      WRITE( LUNIT9 ) IMODAL, METRIK, D9, MSPEDB, ITRNSF                M43.2905
C     READ DATA CARD USING CIMAGE                                       M35.8910
      CALL CIMAGE                                                       M35.8911
      DECODE(80,4029,ABUFF(1)) ( VOLTBC(I), I = 1, 3 )                  M35.8912
 4029 FORMAT( 5E16.0)                                                   M35.8913
C     READ IN THE CURRENT TRANSFORMATION MATRIX   *  *  *  *  *  *  *   M35.8914
      IF( IMODAL .EQ. 0 ) GO TO 4033                                    M35.8915
      DO  4026  J = 1, KTAB                                             M39.3311
         DO  4021  I = 1, KTAB, 6                                       M39.3312
            IJ = I + 5                                                  M35.8918
            IF( IJ .GT. KTAB ) IJ = KTAB                                M39.3313
C     READ DATA CARD USING CIMAGE   *   *   *   *   *   *   *   *   *   M35.8920
            CALL CIMAGE                                                 M35.8921
            DECODE( 80, 4023, ABUFF(1) ) ( TIR(J,KP), KP = I, IJ )      M35.8922
 4023 FORMAT( 6E12.0 )                                                  M35.8923
 4021    CONTINUE                                                       M35.8924
         DO  4022  I = 1, KTAB, 6                                       M39.3314
            IJ = I + 5                                                  M35.8926
            IF( IJ .GT. KTAB ) IJ = KTAB                                M39.3315
C     READ DATA CARD USING CIMAGE   *   *   *   *   *   *   *   *   *   M35.8928
            CALL CIMAGE                                                 M35.8929
            DECODE( 80, 4023, ABUFF(1) ) ( TII(J,KP), KP = I, IJ )      M35.8930
 4022    CONTINUE                                                       M35.8931
CCC      write (*,*) ' subr39.  tir, tii on  lunit9.'                   M43.2906
         WRITE( LUNIT9 ) ( TIR(J,KP), TII(J,KP), KP = 1, KTAB )         M39.3316
 4026 CONTINUE                                                          M35.8933
C     READ MODAL G, B, R, X    *   *   *   *   *   *   *   *   *   *   *M35.8934
C     READ DATA CARD USING CIMAGE                                       M35.8935
 4033 CALL CIMAGE                                                       M35.8936
      DECODE(80,4029,ABUFF(1)) (ALINVC(I), I = 1, 5 )                   M35.8937
      IF( ALINVC(LL4) .LE. 0. )   GO  TO  4034                          M35.8938
CCC      write (*,*) ' subr39.  alinvc on lunit9.'                      M43.2907
      WRITE(LUNIT9) ( ALINVC(I), I = 1, 5 )                             M35.8939
      GO   TO   4033                                                    M35.8940
 4034 REWIND LUNIT9                                                     M35.8941
C     ADVANCE THE CARD READER TO CONFORM TO MISC39'S REQUIREMENTS       M35.8942
      CALL CIMAGE                                                       M35.8943
      JDATCS = 2                                                        M35.8944
C     END KEY-WORD SECTION, PREPARE FOR MISC. DATA CARD:                M32.7761
 4057 DO 4058  I=1, 2                                                   M35.8945
      DO 4058  J=1, 9                                                   M32.7763
 4058 MODSKP(I,J) = 0                                                   M32.7764
      MODESK = 0                                                        M32.7765
      REWIND LUNIT9                                                     M32.7766
CCC      write (*,*) ' subr39.  ready to read imodal, etc. from 9.'     M43.2908
      READ (LUNIT9)  IMODAL, METRIK, D9, MSPEDB, ITRNSF                 M43.2909
      MODIFY = 0                                                        M32.7768
C   ==  READ MISCELLANEOUS DATA CARD BY CALL TO  "MISC39" :    ==       M32.7769
      CALL MISC39                                                       M32.7770
CCC      write (*,*) ' subr39.  Imodal, MODESK, IALDUM, IALTER, KTAB =',M43.2910
CCC     1                       Imodal, MODESK, IALDUM, IALTER, KTAB    M43.2911
      IF ( MODESK  .EQ.  1 )   GO TO 4065                               M32.7771
      DO 4061  I=1, 2                                                   M32.7772
      DO 4061  J=1, 9                                                   M32.7773
 4061 MODSKP(I,J) = 1                                                   M32.7774
 4065 IF ( IALDUM  .NE.  3 )  DIST = D9                                 M43.2912
C     AGAIN, THE USAGE OF 'IALTER' IS RESTORED HERE                     M32.7778
      IF (IALTER .NE. 3 )  DIST = D9                                    M32.7779
      IF ( IMODAL  .EQ.  0  .OR. ITRNSF .EQ. 1)   GO TO 8215            M43.2913
      DO 8203  IP = 1, KTAB                                             M39.3317
      READ (LUNIT9) ( TIR(IP,JP), TII(IP,JP), JP=1, KTAB )              M39.3318
      IF( IPRSUP .LT. 9 ) GO TO 8203                                    M32.7783
      WRITE( LUNIT6, 9436 ) IP, ( TIR(IP,JP), JP = 1, KTAB )            M39.3319
 9436 FORMAT (  4H ROW,  I3,  1H.,  9E13.5  )                           M32.7785
      WRITE( LUNIT6, 9436 ) IP, ( TII(IP,JP), JP = 1, KTAB )            M39.3320
 8203 CONTINUE                                                          M32.7787
C                                                                       M32.7788
C   === MISCELLANEOUS INITIALIZATIONS ===                               M32.7789
 8215 LOUT = LUNIT6                                                     M32.7791
      NMODE = KTAB                                                      M39.3321
      NFITMX = 0                                                                
      IF (NMODE.GT.MMODES) GO TO 2170                                   M32.7794
      CONVUN = 1.D0                                                     M32.7795
      IF (METRIK.EQ.0) CONVUN = 1.60935D0                               M32.7796
      MODIFY = 1                                                        M32.7797
      NEXMIS = 1                                                        M32.7798
      APTDEC = VOLTBC(6)                                                M32.7799
      NPOINT = VOLTBC(4)                                                M32.7800
      IF (NPOINT.GT.MDAPTS) GO TO 2100                                  M32.7801
C   == FIXED PARAMETERS ==                                              M32.7802
C    SEPARATION BETWEEN CONSECUTIVE POLES OR ZEROES (.1%)               M32.7803
      D13 = 1.0 + TENM3                                                 M43.2917
      DEMINP = ALOG1Z ( D13 )                                           M32.7806
C    INITIAL SEPARATION FROM HORIZONTAL LEVEL (1.%)                     M32.7807
      CLEAR = TENM3 * 10.                                               M43.2918
C   == PREPARE IMAGES OF PUNCH FILE ==                                  M32.7810
      KOUNT = 0                                                         M32.7811
      REWIND LUNIT1                                                     M32.7812
      IF ( IPUNCH  .EQ.  0 )                                            M32.7813
     1 WRITE (LUNIT7, 8769)  TCLOCK, DATE1                              M32.7814
 8769 FORMAT (    42HC    PUNCHED CARD OUTPUT OF "JMARTI SETUP",        M35.8946
     1            15H WHICH BEGAN AT,  2X,  2A4,  2X,  2A4  )           M32.7816
      IF ( IPUNCH .NE. 0 )  GO TO 8789                                  M44. 764
      IF ( IMODAL .EQ. 1 ) WRITE (LUNIT7, 8770)                         M44. 765
 8770 FORMAT (53HC   ***** UNTRANSPOSED JMARTI LINE SEGMENT ******    ) M44. 766
      IF ( IMODAL .EQ. 0 .AND. MSPEDB .EQ. 0) WRITE (LUNIT7, 8780)      M44. 767
 8780 FORMAT (53HC   *****  TRANSPOSED JMARTI LINE SEGMENT  ******    ) M44. 768
      IF ( IMODAL .EQ. 0 .AND. MSPEDB .EQ. 1) WRITE (LUNIT7, 8785)      M44. 769
 8785 FORMAT (75HC   ***** SPECIAL DOUBLE CIRCUIT TRANSPOSED JMARTI LINEM44. 770
     1 SEGMENT ******           )                                       M44. 771
 8789 REWIND LUNIT2                                                     M44. 772
      N5 = 0                                                            M32.7818
      IF ( JDATCS .GT. 0 )  GO TO 8777                                  M35.8947
      DO 8774  N12=1, 9999                                              M32.7819
      READ (LUNIT2, 8771)  ( TEXTA6(I), I=1, 14 )                       M32.7820
 8771 FORMAT ( 13A6, A2 )                                               M32.7821
      IF ( IPUNCH  .EQ.  0 )                                            M32.7822
     1 WRITE (LUNIT7, 8772)  ( TEXTA6(I), I=1, 13 )                     M32.7823
 8772 FORMAT ( 2HC ,  13A6 )                                            M32.7824
      IF (IDEBUG.EQ.0) GO TO 8100                                       M32.7825
      WRITE (LUNIT1, 8773)  ( TEXTA6(I), I=1, 14 )                      M32.7826
 8773 FORMAT ( 3H C ,  13A6,  A2 )                                      M32.7827
      KOUNT = KOUNT+1                                                   M32.7828
 8100 IF ( TEXTA6(1) .EQ. BLANK     .AND.                               M32.7829
     1     TEXTA6(2) .EQ. BLANK )   N5 = N5 + 1                         M32.7830
      IF ( N5  .GE.  2 )   GO TO 8777                                   M32.7831
 8774 CONTINUE                                                          M32.7832
C                                                                       M32.7833
C             MODES FITTING LOOP                                        M32.7834
C                                                                       M32.7835
 8777 D13 = 1.005D0                                                    
      D1LIM = ALOG1Z ( D13 )                                               
      D13 = 1.0025D0                                                    
      D2LIM = ALOG1Z ( D13 )                                             
      DO 3300 IMODE = 1,NMODE                                           M32.7836
      REWIND LUNIT9                                                     M43.2919
C  ??? DUMMY READ TO SKIP HEADER ???                                    M43.2920
      READ(LUNIT9) ID1,ID1,D1, ID1, ID1                                 M43.2921
      IF ( IMODAL .EQ. 0  .OR. ITRNSF .EQ. 1)  GO TO 3205               M43.2922
      DO 3230 IP = 1, KTAB                                              M43.2923
 3230 READ(LUNIT9) (TDUM(IP,JP),TDUM(IP,JP),JP=1,KTAB)                  M43.2924
 3205 IF (ITRNSF .EQ. 1)                                                        
     1  READ(LUNIT9) (LLTEMP(I), I =1,KTAB)                             M43.2926
C  ??? END OF DUMMY READ ???                                            M43.2927
      DO 3220 IPOINT = 1, NPOINT                                        M43.2928
      DO 3215 I = 1, NMODE                                              M43.2929
      READ (LUNIT9) TWOPIF, YCHARM, YCHARA, ALPHA,BETA                  M43.2930
      IF ( ITRNSF .NE. 1 )  GO TO 3213                                  M43.2931
      READ (LUNIT9) (TIR(J,I),TII(J,I), J=1,NMODE)                      M43.2932
 3213 IF ( IMODE .NE. I )  GO TO 3215                                   M43.2933
      if ( NFITMX .eq. 1 )  go to 8210                                          
      INDEX = (IPOINT-1) * 5                                            M44. 774
      ALINTP(INDEX+1) = TWOPIF                                          M43.2935
      ALINTP(INDEX+2) = YCHARM                                          M43.2936
      ALINTP(INDEX+3) = YCHARA                                          M43.2937
      ALINTP(INDEX+4) = ALPHA                                           M43.2938
      ALINTP(INDEX+5) = BETA                                            M43.2939
      IF (IPRSUP .LE. 1 )   GO TO 3215                                  M44. 775
      write(LUNIT6,*)' %% in subr39, omega, ychar(mag,ang),alpha,beta', M44. 776
     1 ALINTP(INDEX+1),ALINTP(INDEX+2),ALINTP(INDEX+3),                 M44. 777
     2 ALINTP(INDEX+4),ALINTP(INDEX+5)                                  M44. 778
      GO TO 3215                                                        M44. 779
 8210 index = (2*nmode+1) * ( ipoint-1 )                                M44. 780
      alintp(index+1) = twopif                                          M44. 781
      DO 3214 J = 1, NMODE                                              M43.2941
      L = 2 * (J-1) + 1                                                 M43.2942
      ALINTP(INDEX+L+1) = TIR(J,I)                                              
      ALINTP(INDEX+L+2) = TII(J,I)                                      M44. 783
 3214 CONTINUE                                                          M43.2945
      IF( IPRSUP .GE. 1)                                                M44. 784
     1 WRITE(LUNIT6,*) ' Ti for column ', IMODE, 'are ',                M43.2952
     1                 ( TIR(J,I),TII(J,I), J=1,NMODE)                  M43.2953
 3215 CONTINUE                                                          M43.2954
 3220 CONTINUE                                                          M43.2955
C             YC & A1 & Ti FUNCTIONS                                    M43.2956
      NCURVE = 2                                                        M43.2957
      IF ( NFITMX .EQ. 1 )  NCURVE = NMODE                              M44. 785
      DO 123 I = 1, NCURVE                                              M43.2959
  123 MINUST(I) = 0                                                     M43.2960
      DO 3200 ICURVE = 1, NCURVE                                        M43.2961
      CALL TIME44 ( TCLOCK(1) )                                         M43.2962
      WRITE (LUNIT6, 6301)  TCLOCK, IMODE, NCURVE                       M43.2963
 6301 FORMAT ( ' Begin modal fit at  ',  2A4,                           M43.2964
     1         '.     IMODE, NCURVE =',  2I5 )                          M43.2965
      NUMONE = 0                                                        M43.2966
      NUMZRO = 0                                                        M43.2967
      IFTYPE = ICURVE                                                   M32.7841
      IF ( NFITMX .EQ. 1 )  IFTYPE = 1                                  M43.2968
C                                                                       M32.7842
C             READ PARAMETERS                                           M32.7843
C                                                                       M32.7844
      IF (ICURVE.EQ.2 .AND. NFITMX .EQ. 0) GO TO 3150                   M44. 786
      IF (IMODE.NE.NEXMIS) GO TO 3152                                   M32.7846
      CALL MISC39                                                       M32.7847
      EPSTZC = EPSTOL                                                   M32.7848
      NORMZC = NORMAX                                                   M32.7849
      IECOZC = IECODE                                                   M32.7850
      IFWTZC = IFWTA                                                    M32.7851
      IFPLZC = IFPLOT                                                   M32.7852
      IFDAZC = IFDAT                                                    M32.7853
      INELZC = INELIM                                                   M32.7854
 3152 IF (MODSKP(1,IMODE).EQ.0) GO TO 3200                              M32.7855
      IF ( NFITMX .EQ. 0 )  GO TO 3156                                  M44. 787
      WRITE(LUNIT6, 3155) ICURVE, IMODE                                 M44. 788
 3155 FORMAT (//,' @@@ BEGIN Ti FITTING FOR ELEMENT:', 2I2, 3X, '@@@'  )M43.2972
      IF ( LLTEMP(IMODE) .NE. ICURVE )  GO TO 3160                      M44. 789
      WRITE(LUNIT6,*) ' This Tij = (1.,0.)  '                           M43.2974
      GO TO 8792                                                        M43.2975
 3156 WRITE(LUNIT6,7007) IMODE                                         
 7007 FORMAT(//,1X,31H@@@ BEGIN  Yc  FITTING FOR MODE,I3,3X, 3H@@@,1X)  M43.2977
      GO TO 3160                                                        M32.7858
 3150 IF (IMODE.NE.1) GO TO 3170                                        M32.7859
      IF (NEXMIS.EQ.-1) GO TO 3170                                      M32.7860
      WRITE(LUNIT6,7009) NEXMIS                                        
 7009 FORMAT(//,1X,39H*** ERROR STOP. USER HAS NOT REQUESTED ,          M33.2186
     1             38HTHE READING OF MISCELLANEOUS DATA FOR ,           M33.2187
     2             30HMODE 1 OF A1 FITTING. NEXMIS =,I5)                M33.2188
      CALL STOPTP                                                               
 3170 IF (IMODE.NE.-NEXMIS) GO TO 3180                                  M32.7866
C     THIS MODE OF A1 HAS NEW MISCELLANEOUS DATA:                       M32.7867
      MODIFY = 2                                                        M32.7868
      CALL MISC39                                                       M32.7869
      EPSTA1 = EPSTOL                                                   M32.7870
      NORMA1 = NORMAX                                                   M32.7871
      IECOA1 = IECODE                                                   M32.7872
      IFWTA1 = IFWTA                                                    M32.7873
      IFPLA1 = IFPLOT                                                   M32.7874
      IFDAA1 = IFDAT                                                    M32.7875
      INELA1 = INELIM                                                   M32.7876
 3180 IF (MODSKP(2,IMODE).EQ.0) GO TO 3200                              M32.7877
      WRITE(LUNIT6,7012) IMODE                                         
 7012 FORMAT(//,1X,31H@@@ BEGIN  A1  FITTING FOR MODE,I3,3X, 3H@@@,1X)  M33.2189
 3160 IF (ICURVE.EQ.2 .AND. NFITMX .EQ. 0) GO TO 3162                   M44. 790
      EPSTOL = EPSTZC                                                   M32.7881
      NORMAX = NORMZC                                                   M32.7882
      IECODE = IECOZC                                                   M32.7883
      IFWTA = IFWTZC                                                    M32.7884
      IFPLOT = IFPLZC                                                   M32.7885
      IFDAT = IFDAZC                                                    M32.7886
      INELIM = INELZC                                                   M32.7887
      GO TO 3164                                                        M32.7888
 3162 EPSTOL = EPSTA1                                                   M32.7889
      NORMAX = NORMA1                                                   M32.7890
      IECODE = IECOA1                                                   M32.7891
      IFWTA = IFWTA1                                                    M32.7892
      IFPLOT = IFPLA1                                                   M32.7893
      IFDAT = IFDAA1                                                    M32.7894
      INELIM = INELA1                                                   M32.7895
 3164 IF (NORMAX.LE.MPOLES) GO TO 3165                                  M32.7896
      WRITE(LUNIT6,7015) NORMAX,MPOLES                                 
 7015 FORMAT(//,1X,29H%%% SPECIFIED MAXIMUM ORDER (,I3,                 M33.2190
     1   28H) EXCEEDS DIMENSIONS LIMIT (,I3, 2H).,/,1X,                 M33.2191
     2   49HMAXIMUM ORDER WILL BE SET EQUAL TO THIS LIMIT %%%,1X)       M33.2192
      NORMAX = MPOLES                                                   M32.7901
C                                                                       M32.7902
C             READ DATA POINTS                                          M32.7903
C                                                                       M32.7904
 3165 IF (IFDAT.EQ.0) GO TO 3210                                        M32.7905
      WRITE(LUNIT6,7017)                                               
 7017 FORMAT(//,1X,30X,34H###  D A T A  F U N C T I O N  ###)           
      IF (ICURVE .gE. 2 .OR. NFITMX .EQ. 1) GO TO 3167                  M44. 791
      WRITE(LUNIT6,7019)                                               
 7019 FORMAT(//,1X,                                                     M32.7910
     1        ' UNITS: FREQ IN HZ; Yc IN MHOS, PHYc IN DEGREES' )       M43.2979
      WRITE(LUNIT6,7021)                                               
 7021 FORMAT(/,1X,4X, 4HFREQ,8X,  2HYc,8X, 4HPHYc,7X, 4HFREQ)           
      GO TO 3210                                                        M32.7918
 3166 WRITE(LUNIT6,7024)                                               
 7024 FORMAT(//,1X,'UNITS: FREQ IN HZ; Ti UNITLESS, PHTi IN DEGREES' )  
      WRITE(LUNIT6,7027)                                               
 7027 FORMAT(/,1X,4X, 4HFREQ,8X,  2HTi,8X, 4HPHTi,7X, 4HFREQ)           
      GO TO 3210                                                        M43.2985
 3167 IF ( NFITMX .EQ. 1 )  GO TO 3166                                  M44. 792
      WRITE(LUNIT6,7029)                                               
 7029 FORMAT(//,1X,                                                     
     1        ' UNITS: FREQ IN HZ; VELOCITY IN KM/SEC, TRAVELING TIME INM43.2988
     2MILLISEC, A1 IN PER UNIT, PHA1 IN DEGREES' )                      M43.2989
      WRITE(LUNIT6,7031)                                               
 7031 FORMAT(/,5X, 4HFREQ,4X, 8HVELOCITY,3X, 9HTRAV.TIME,5X, 2HA1,8X,   
     1 4HPHA1,7X, 4HFREQ)                                               M43.2991
 3210 DO 3100 IPOINT = 1,NPOINT                                         M43.2992
      if (ipoint.eq.1)     f0=alinTP(1)                                 M43.2993
      INDEX = (IPOINT-1) *  5                                           M44. 793
      IF ( NFITMX .EQ. 1 )  INDEX =( 2*NMODE + 1 )* (IPOINT-1)          M44. 794
      FDAT = ALINTP(INDEX+1)                                            M32.7945
      IF (ICURVE .GE. 2 .OR. NFITMX .EQ. 1) GO TO 3110                  M44. 795
      ADAT = ALINTP(INDEX+2)                                            M43.2996
C  === VALUE OF G DIRECTLY IN THE MODAL DOMAIN ===                      M32.7949
C     IF (GDAT.EQ.0.) GDAT = GMODE/CONVUN                               M43.2997
      PHDAT = ALINTP(INDEX+3)                                           M43.2998
C  === EVALUATE LINE FUNCTIONS ===                                      M32.7952
      IF (ICURVE .GE. 2 .OR. NFITMX .EQ. 1) GO TO 3110                  M44. 796
 3102 IF (IFDAT.EQ.0) GO TO 3120                                        M43.3000
      D3 = PHDAT * 360. / TWOPI                                         M32.7961
      WRITE(LUNIT6,7034) FDAT,ADAT,D3,FDAT                             
 7034 FORMAT(1X,4D11.4)                                                 
      GO TO 3120                                                        M32.7964
 3104 L = INDEX + 2 * (ICURVE-1) + 2                                    M44. 797
      ADAT = ALINTP( L )                                                M43.3004
      IF ( IPOINT .EQ. 1 )  ASAVE = ADAT * TENM3                        M43.3005
      IF ( ADAT .LT. ASAVE ) GO TO 3130                                 M43.3006
      PHDAT = ALINTP(L + 1 )                                            M43.3007
      IF (IFDAT.EQ.0) GO TO 3103                                        M43.3008
      D3 = PHDAT * 360. / TWOPI                                         M43.3009
      WRITE(LUNIT6,7034) FDAT,ADAT,D3,FDAT                           
 3103 IF ( ABSZ(ADAT - 1.) .LT. EPSILN )  NUMONE = NUMONE + 1           M43.3011
C     IF ( (180. - ABSZ( PHDAT*360./TWOPI )) .GT. 10. )  GO TO 3120     M43.3012
      IF ( MINUST(ICURVE) .EQ. 1 ) GO TO 3105                           M43.3013
      IF ( (PAI - ABSZ(PHDAT)) .GT. TENDEG )  GO TO 3106                M43.3014
      IF ( IPOINT .GT. 1 )  GO TO 3106                                  M43.3015
      MINUST(ICURVE) = 1                                                        
 3105 PHDAT = PHDAT + PAI                                               M43.3017
 3106 IF ( ADAT .LT. TENM3)  NUMZRO = NUMZRO + 1                        M43.3018
      GO TO 3120                                                        M43.3019
C  ### EVALUATION OF THE PROPAGATION FUNCTION A1 ###                    M32.7965
 3110 IF (NFITMX .EQ. 1 )  GO TO 3104                                   M44. 798
      ALPHA = ALINTP(INDEX+4)                                           M43.3021
      BETA = ALINTP(INDEX+5)                                            M43.3022
      IF ( IPRSUP .GE. 1 )                                              M43.3023
     1 write (lunit6,*) ' alpha, beta =', alpha,beta                    M43.3024
      ADAT = EXPZ(-ALPHA*DIST)                                          M32.7970
      IF (ADAT.GT.1.5D0) GO TO 3132                                     M32.7971
      IF (ADAT.LT.AMINA1) GO TO 3130                                    M32.7972
      PHDAT = -BETA*DIST                                                M32.7973
      IF (IFDAT.EQ.0) GO TO 3120                                        M32.7974
      D3 = FDAT/BETA                                                    M32.7977
      D4 = (DIST/D3)*1.D3                                               M32.7978
      D5 = PHDAT*360.D0/TWOPI                                           M32.7979
      WRITE(LUNIT6,7039) FDAT,D3,D4,ADAT,D5,FDAT                       
 7039 FORMAT(1X,6D11.4)                                                 
 3120 D11 = FDAT/TWOPI                                                  M32.7983
      XDAT(IPOINT) = ALOG1Z(D11)                                        M32.7984
      YDAT(IPOINT) = ALOG1Z(ADAT)                                       M32.7985
      APHDAT(IPOINT) = PHDAT                                            M32.7986
 3100 CONTINUE                                                          M32.7987
      N9 = .75 * NPOINT                                                 M43.3027
      IF ( NUMONE .LT.  N9 )  GO TO 3128                                M43.3028
      WRITE(LUNIT6,*) ' This is a near-one Tij element. '               M43.3029
      GO TO 8792                                                        M43.3030
 3128 IF ( NUMZRO .GE.  N9 )  GO TO 8793                                        
C                                                                       M32.7988
      NDATA = NPOINT                                                    M32.7989
      GO TO 3140                                                        M32.7990
 3130 NDATA = IPOINT-1                                                  M32.7991
      GO TO 3140                                                        M32.7992
 3132 WRITE(LUNIT6,7037) IMODE                                         
 7037 FORMAT(//,1X,                                                     
     1 25X,        41H??????????  A T T E N T I O N  ??????????,        M33.2209
     2 /,1X,   50HTHE MAGNITUDE OF THE LINE PROPAGATION FUNCTION (A1,   M33.2210
     3         10H) FOR MODE,I3,3X,29HATTAINS VALUES LARGER THAN 1.,    M33.2211
     4 /,1X,   50HTHIS CONDITION IS PROBABLY CAUSED BY THE LIMITATIO,   M33.2212
     5         50HNS OF USING A CONSTANT COMPLEX TRANSFORMATION MATR,   M33.2213
     6          2HIX,                                                   M33.2214
     7 /,1X,   50HTO DIAGONALIZE THE LINE MATRICES AT ALL FREQUENCIE,   M33.2215
     8         50HS. NEVERTHELESS, THE FUNCTION WILL BE FIT UP TO A ,   M33.2216
     9 /,1X, 23HMAGNITUDE VALUE OF 1.5.)                                M33.2217
      WRITE(LUNIT6,7041)                                               
 7041 FORMAT(1X,                                                        
     1         50HHOWEVER, MORE REALISTIC RESULTS COULD PROBABLY BE ,   M33.2218
     2         50HOBTAINED BY USING ONLY THE REAL PART OF THE TRANSF,   M33.2219
     3          8HORMATION,                                             M33.2220
     4 /,1X,   50HMATRIX (OPTION -2 IN FREQUENCY LOOP CARD IN LINE C,   M33.2221
     5         15HONSTANTS DATA).)                                      M33.2222
      NDATA = IPOINT-1                                                  M32.8011
C   == TRAVELLING TIME OF HIGHEST FREQ. POINT ==                        M32.8012
 3140 IF (ICURVE.EQ.2 .AND. NFITMX .EQ. 0)                              M44. 799
     1 TRAVHF = DIST*BETA/FDAT                                          M32.8014
C                                                                       M32.8015
C             DEFINE CURVE ZONES                                        M32.8016
C                                                                       M32.8017
      IKNEE = 1                                                         M32.8023
      XKNEE(1) = XDAT(1)                                                M32.8024
      YLEVL0 = YDAT(1)                                                  M32.8025
      I = 1
  120 I = I + 1
      IF (I .GT. NDATA) GO TO 230                                  
      IF (YDAT(I).LT.(YLEVL0-D1LIM)) GO TO 130                    
      IF (YDAT(I).GT.(YLEVL0+D1LIM)) GO TO 180                    
      GO TO 120                                                         M32.8031
C  LOOK FOR VALLEY                                                      M32.8032
  130 YLEVL0 = YDAT(I)                                               
      X1LEVL = XDAT(I)                                                
  140 I = I + 1
      IF (I.GT.NDATA) GO TO 230                                 
      IF (YDAT(I).LT.(YLEVL0-D1LIM)) GO TO 130               
      IF (YDAT(I).GT.(YLEVL0+D1LIM)) GO TO 150                       
      GO TO 140                                                         M32.8039
  150 ISCAPE = I                                                       
  160 I = I - 1      
      IF (YDAT(I).LE.(YLEVL0+D2LIM)) GO TO 170                      
      GO TO 160                                                         M32.8043
  170 X2LEVL = XDAT(I)
      IKNEE = IKNEE+1                                                   M32.8045
      IF (IKNEE.GT.MXKNEE) GO TO 2110                                   M32.8046
      XLEVL = (X1LEVL+X2LEVL) * ONEHAF                                  M43.3033
      XKNEE(IKNEE) = XLEVL                                              M32.8048
C  LOOK FOR PEAK                                                        M32.8049
      I = ISCAPE    
  180 YLEVL0 = YDAT(I)                                              
      X1LEVL = XDAT(I)
  190 I = I + 1
      IF (I.GT.NDATA) GO TO 230                                  
      IF (YDAT(I).GT.(YLEVL0+D1LIM)) GO TO 180                          
      IF (YDAT(I).LT.(YLEVL0-D1LIM)) GO TO 200                          
      GO TO 190                                                         M32.8057
  200 ISCAPE = I                                                        
  210 I = I - 1
      IF (YDAT(I).GE.(YLEVL0-D2LIM)) GO TO 220                     
      GO TO 210                                                         M32.8061
  220 X2LEVL = XDAT(I)
      IKNEE = IKNEE+1                                                   M32.8063
      IF (IKNEE.GT.MXKNEE) GO TO 2110                                   M32.8064
      XLEVL = (X1LEVL+X2LEVL) * ONEHAF                                  M43.3035
      XKNEE(IKNEE) = XLEVL                                              M32.8066
      I = ISCAPE                                                        
      GO TO 130                                                         M32.8068
  230 IKNEE = IKNEE+1                                                   M32.8069
      IF (IKNEE.GT.MXKNEE) GO TO 2110                                   M32.8070
      XKNEE(IKNEE) = XDAT(NDATA)                                        M32.8071
      IF (IDEBUG.EQ.0) GO TO 250                                        M32.8072
      WRITE(LUNIT6,7044)                                               
 7044 FORMAT(/,1X,13X,19H--- CURVE ZONES ---,1X)                        
      DO 240 I=1,IKNEE                                                  M32.8075
      WRITE(LUNIT6,7047) I,XKNEE(I)                                    
 7047 FORMAT(/,10X,I10,F10.3)                                           
  240 CONTINUE                                                          M32.8078
  250 NZONE = IKNEE-1                                                   M32.8079
C   == PARAMETERS FOR LEAST SQUARES ERROR CHECKING ==                   M32.8080
      DXCOMP = .05D0                                                    M32.8081
      NECHK1 = NDATA/(APTDEC*DXCOMP)+ONEHAV                             M32.8082
      XBEGR = XDAT(1)                                                   M32.8083
      XENDR = XKNEE(2)                                                  M32.8084
      D1 = YDAT(1)                                                      M32.8085
      REFB = YFUN39(XENDR)                                              M43.3037
      D3 = .001D0                                                       M32.8087
      D4 = REFB - D1                                                    M43.3038
      D5 = 10.D0 ** D4                                                  M43.3039
      YCUT = D1 + ALOG1Z( UNITY+D3*(D5-UNITY) )                         M43.3040
      YCUTPU = (YCUT-D1)/(REFB-D1)                                      M43.3041
      REFA = YFUN39(XBEGR)                                              M43.3042
      CALL SPLIT(XBEGR,XENDR,YCUTPU,XBEGTL)                             M32.8092
      XBEGR = XDAT(NDATA)                                               M32.8093
      XENDR = XKNEE(IKNEE-1)                                            M32.8094
      D1 = YDAT(NDATA)                                                  M32.8095
      REFB = YFUN39(XENDR)                                              M43.3043
      IF (ICURVE.EQ.2 .AND. NFITMX .EQ. 0) D3 = .25D0                   M44. 800
      D4 = REFB - D1                                                    M43.3044
      D5 = 10.D0 ** D4                                                  M43.3045
      YCUT = D1 + ALOG1Z( UNITY+D3*(D5-UNITY) )                         M43.3046
      YCUTPU = (YCUT-D1)/(REFB-D1)                                      M43.3047
      REFA = YFUN39(XBEGR)                                              M43.3048
      CALL SPLIT(XBEGR,XENDR,YCUTPU,XENDTL)                             M32.8102
      I1 = APTDEC*(XBEGTL-XDAT(1))+1.D0                                 M32.8103
      I2 = APTDEC*(XENDTL-XDAT(1))+2.D0                                 M32.8104
      NECHK2 = (I2-I1+1)/(APTDEC*DXCOMP)+ONEHAV                         M32.8105
C                                                                       M32.8106
C             MAIN LOOP FOR ALL-SEGMENTS ALLOCATION                     M32.8107
C                                                                       M32.8108
      ITERLP = 0                                                        M32.8109
      INCRTL = 0                                                        M32.8110
      ERROPT = 1.D12                                                    M32.8111
      ICHKP = 0                                                         M32.8112
      NPOLE = 0                                                         M32.8113
      IF (NORMAX.GT.10) GO TO 252                                       M32.8114
      TOLFAC = 3.0D0                                                    M32.8115
      TOLMIN = .70D0                                                    M32.8116
      GO TO 260                                                         M32.8117
  252 TOLFAC = 2.0D0                                                    M32.8118
      TOLMIN = .20D0                                                    M32.8119
  260 ITERLP = ITERLP+1                                                 M32.8120
      IF (IDEBUG.GE.1)                                                  M32.8121
     1 WRITE(LUNIT6,7049) ITERLP                                       
 7049 FORMAT(//,2X,38H###  A L L O C A T I O N  L O O P  NO.,I3,4X,     
     1 3H###,1X)                                                        M33.2237
  265 NTOTRA = 0                                                        M32.8124
      DO 440 IZONE=1,NZONE                                              M32.8125
      XBEGZ = XKNEE(IZONE)                                              M32.8126
      XENDZ = XKNEE(IZONE+1)                                            M32.8127
      IF (YFUN39(XBEGZ).GT.YFUN39(XENDZ)) GO TO 270                     M33.2238
C   POSITIVE SLOPE ZONE                                                 M32.8129
      SIGNZ = 1.                                                        M32.8130
      GO TO 280                                                         M32.8131
C   NEGATIVE SLOPE ZONE                                                 M32.8132
  270 SIGNZ = -1.                                                       M32.8133
C   ALLOCATE SLOPES IN ZONE                                             M32.8134
  280 IOPRAU = 0                                                        M32.8135
      ICHKRA = 1                                                        M32.8136
      NCHKRA = 1                                                        M32.8137
      XCHKRA(1) = XBEGZ                                                 M32.8138
      XCHKRA(2) = XENDZ                                                 M32.8139
C                                                                       M32.8140
  290 ID = 2*ICHKRA                                                     M32.8141
      XBEGR = XCHKRA(ID-1)                                              M32.8142
      XENDR = XCHKRA(ID)                                                M32.8143
      REFA = YFUN39(XBEGR)                                              M43.3049
      REFB = YFUN39(XENDR)                                              M43.3050
      XCLA = 0.                                                         M32.8144
      XCLB = 0.                                                         M32.8145
      XLIM1 = XBEGR                                                     M32.8146
      XLIM2 = XENDR                                                     M32.8147
      IF (XBEGR.EQ.XBEGZ)                                               M43.3051
     1 CALL SPLIT (XBEGR,XENDR,CLEAR,XCLA)                              M43.3052
      IF (XENDR.NE.XENDZ) GO TO 310                                     M43.3053
      REFA = YFUN39(XENDR)                                              M43.3054
      REFB = YFUN39(XBEGR)                                              M43.3055
      CALL SPLIT (XENDR,XBEGR,CLEAR,XCLB)                               M32.8151
      D1 = REFA                                                         M43.3056
      REFA = REFB                                                       M43.3057
      REFB = D1                                                         M43.3058
  310 IF (XCLA.NE.0.) XLIM1 = XCLA                                      M32.8152
      IF (XCLB.NE.0.) XLIM2 = XCLB                                      M32.8153
      IF (IDEBUG.GE.3)                                                  M32.8154
     1 WRITE(LUNIT6,7051) XBEGR,XENDR                                  
 7051 FORMAT(//,1X, 7HREGION:,3X, 5HXBEG=,E11.4,3X, 5HXEND=,E11.4,/)    
CCC      write (*,*) ' after #130 XLIM2, XLIM1  ', XLIM2 , XLIM1        M43.3059
      ABETA = ( REFB - REFA ) / (XLIM2-XLIM1)                           M43.3060
      NALPH1 = ABETA                                                    M32.8158
      ALPHA = NALPH1+SIGNZ                                              M32.8159
      CALL LOCSL (XBEGR,XENDR,ALPHA,XMIDR,XCORNA,XCORNB,ERYMAX)         M32.8160
CCC      write(*,*) 'IZONE, ICHKRA, XMIDR, XENDR after call LOCSL  ',   M43.3061
CCC     1 IZONE, ICHKRA, XMIDR, XENDR                                   M43.3062
      IF ( ABSZ(XMIDR-XENDR) .GT. 0.001D0 )  GO TO 313                  M43.3063
      IF ( XENDR .EQ. XENDZ ) GO TO 440                                 M43.3064
      ICHKRA = ICHKRA + 1                                                       
      GO TO 290                                                                 
  313 TOLERA = 14.5938964D0+3.26036143D0*ALOGZ(ABSZ(ABETA))             M43.3067
      IF (TOLERA.LT.TOLMIN) TOLERA = TOLMIN                             M32.8162
      TOLERA = TOLFAC*TOLERA                                            M32.8163
      D13 = 1.0 + TOLERA / 100.                                         M32.8164
      TOLERA = ALOG1Z ( D13 )                                           M32.8165
      IF (IDEBUG.LT.3) GO TO 315                                        M32.8166
      D1 = 100.*(10.**ERYMAX-1.)                                        M32.8167
      D2 = 100.*(10.**TOLERA-1.)                                        M32.8168
      WRITE(LUNIT6,7054) XMIDR,XCORNA,XCORNB,ABETA,ALPHA,D1,D2         
 7054 FORMAT(/,1X, 6HXMIDR=,E11.4,3X,                                   
     1        7HXCORNA=,E11.4,3X, 7HXCORNB=,E11.4,/,1X,                 M33.2242
     2        7HABETA =,F7.2,4X, 7HALPHA =,F7.2,/,                      M33.2243
     3       1X, 7HERYMAX=,F7.2, 1H%,3X, 7HTOLERA=,F7.2, 1H%,1X)        M33.2244
  315 IF (ERYMAX.LT.TOLERA) GO TO 330                                   M32.8174
C   DIVIDE REGION INTO TWO SUBREGIONS                                   M32.8175
      ID = 2*(NCHKRA+1)                                                 M32.8176
      NCHKRA = NCHKRA+2                                                 M32.8177
      IF (2*NCHKRA.LE.MXCHKR) GO TO 320                                 M32.8178
      IF (ITERLP.EQ.1) GO TO 2120                                       M32.8179
      GO TO 790                                                         M32.8180
  320 XCHKRA(ID-1) = XBEGR                                              M32.8181
      XCHKRA(ID)   = XMIDR                                              M32.8182
      XCHKRA(ID+1) = XMIDR                                              M32.8183
      XCHKRA(ID+2) = XENDR                                              M32.8184
      ICHKRA = ICHKRA+1                                                 M32.8185
      GO TO 290                                                         M32.8186
C   STORE PARAMETERS, DO NOT SUBDIVIDE                                  M32.8187
  330 IOPRAU = IOPRAU+1                                                 M32.8188
      IF (4*IOPRAU.LE.MZOPRA) GO TO 340                                 M32.8189
      IF (ITERLP.EQ.1) GO TO 2130                                       M32.8190
      GO TO 790                                                         M32.8191
  340 ID = 4*IOPRAU                                                     M32.8192
      ZOPRAU(ID-3) = XBEGR                                              M32.8193
      ZOPRAU(ID-2) = XCORNA                                             M32.8194
      ZOPRAU(ID-1) = XCORNB                                             M32.8195
      ZOPRAU(ID)   = ALPHA                                              M32.8196
      IF(ICHKRA.EQ.NCHKRA) GO TO 350                                    M32.8197
      ICHKRA = ICHKRA+1                                                 M32.8198
      GO TO 290                                                         M32.8199
C                                                                       M32.8200
  350 IF (IDEBUG.LT.3) GO TO 360                                        M32.8201
      WRITE(LUNIT6,7057)                                               
 7057 FORMAT(//,16X,22H--- RANGES CHECKED ---,/)                        
      DO 353 I = 1,NCHKRA                                               M32.8204
      ID = 2*I                                                          M32.8205
      WRITE(LUNIT6,7059) XCHKRA(ID-1),XCHKRA(ID)                       
 7059 FORMAT(15X,2E11.4)                                                
  353 CONTINUE                                                          M32.8208
      WRITE(LUNIT6,7061)                                               
 7061 FORMAT(/,5X,44H----- RANGES IN ZONE VECTOR. UNORDERED -----)      
      WRITE(LUNIT6,7064)                                               
 7064 FORMAT(/,10X, 5HXBEGR,6X, 6HXCORNA,6X, 6HXCORNB,4X, 5HALPHA)      
      DO 356 I = 1,IOPRAU                                               M32.8213
      ID = 4*I                                                          M32.8214
      WRITE(LUNIT6,7067) ZOPRAU(ID-3),ZOPRAU(ID-2),                    
     1                  ZOPRAU(ID-1),ZOPRAU(ID)                         M32.8216
 7067 FORMAT(5X,3E12.4,F6.0)                                            
  356 CONTINUE                                                          M32.8218
C             SORT RANGES OF IDENTIFIED REGIONS                         M32.8219
  360 NOPRAO(IZONE) = IOPRAU                                            M32.8220
      IF (IOPRAU.EQ.1) GO TO 420                                        M32.8221
C   FILL IN AUXILIARY BUFFER                                            M32.8222
      DO 370 I=1,IOPRAU                                                 M32.8223
      ID1 = 2*I-1                                                       M32.8224
      ID2 = 4*I-3                                                       M32.8225
      XAUXD(ID1) = ZOPRAU(ID2)                                          M32.8226
      XAUXD(ID1+1) = I                                                  M32.8227
  370 CONTINUE                                                          M32.8228
C   SORT AUXILIARY BUFFER                                               M32.8229
      K=IOPRAU                                                          M32.8230
      IFLAG = K                                                         M32.8231
  380 IF (IFLAG.LE.0) GO TO 400                                         M32.8232
      K = IFLAG-1                                                       M32.8233
      IFLAG = 0                                                         M32.8234
      DO 390 J=1,K                                                      M32.8235
      ID = 2*J-1                                                        M32.8236
      IF (XAUXD(ID).LE.XAUXD(ID+2)) GO TO 390                           M32.8237
      XD1 = XAUXD(ID)                                                   M32.8238
      XD2 = XAUXD(ID+1)                                                 M32.8239
      XAUXD(ID) = XAUXD(ID+2)                                           M32.8240
      XAUXD(ID+1) = XAUXD(ID+3)                                         M32.8241
      XAUXD(ID+2) = XD1                                                 M32.8242
      XAUXD(ID+3) = XD2                                                 M32.8243
      IFLAG = J                                                         M32.8244
  390 CONTINUE                                                          M32.8245
      GO TO 380                                                         M32.8246
C             FILL-IN ORDERED VECTOR WITH ALL-RANGES INFORMATION        M32.8247
  400 N1 = NTOTRA                                                       M32.8248
      NTOTRA = NTOTRA+IOPRAU                                            M32.8249
      IF (4*NTOTRA.LE.MZOPRA) GO TO 405                                 M32.8250
      IF (ITERLP.EQ.1) GO TO 2140                                       M32.8251
      GO TO 790                                                         M32.8252
  405 DO 410 I=1,IOPRAU                                                 M32.8253
      ID1 = 4*(N1+I)-3                                                  M32.8254
      ID = XAUXD(2*I)+ONEHAV                                            M32.8255
      ID2 = 4*ID-3                                                      M32.8256
      ZOPRAO(ID1) = ZOPRAU(ID2)                                         M32.8257
      ZOPRAO(ID1+1) = ZOPRAU(ID2+1)                                     M32.8258
      ZOPRAO(ID1+2) = ZOPRAU(ID2+2)                                     M32.8259
      ZOPRAO(ID1+3) = ZOPRAU(ID2+3)                                     M32.8260
  410 CONTINUE                                                          M32.8261
      GO TO 440                                                         M32.8262
  420 N1 = NTOTRA                                                       M32.8263
      NTOTRA = NTOTRA+IOPRAU                                            M32.8264
      IF (4*NTOTRA.LE.MZOPRA) GO TO 425                                 M32.8265
      IF (ITERLP.EQ.1) GO TO 2140                                       M32.8266
      GO TO 790                                                         M32.8267
  425 ID1 = 4*(N1+1)-3                                                  M32.8268
      ZOPRAO(ID1)   = ZOPRAU(1)                                         M32.8269
      ZOPRAO(ID1+1) = ZOPRAU(2)                                         M32.8270
      ZOPRAO(ID1+2) = ZOPRAU(3)                                         M32.8271
      ZOPRAO(ID1+3) = ZOPRAU(4)                                         M32.8272
  440 CONTINUE                                                          M32.8273
C                                                                       M32.8274
      IF (IDEBUG.LT.2) GO TO 450                                        M32.8275
      WRITE(LUNIT6,7069)                                               
 7069 FORMAT(/,6X,40H-----  ALL-RANGES VECTOR. ORDERED  -----)          
      WRITE(LUNIT6,7071)                                               
 7071 FORMAT(/,10X, 5HXBEGR,6X, 6HXCORNA,6X, 6HXCORNB,4X, 5HALPHA)      
      DO 445 I = 1,NTOTRA                                               M32.8280
      ID = 4*I                                                          M32.8281
      WRITE(LUNIT6,7074) ZOPRAO(ID-3),ZOPRAO(ID-2),                    
     1                  ZOPRAO(ID-1),ZOPRAO(ID)                         M32.8283
 7074 FORMAT(1X,4X,3E12.4,F6.0)                                         
  445 CONTINUE                                                          M32.8285
C  CHECK NUMBER OF POLES                                                M32.8286
  450 NPOLES = 0                                                        M32.8287
      DO 460 IRANGE = 1,NTOTRA                                          M32.8288
      ALPHA = ZOPRAO(4*IRANGE)                                          M32.8289
      JN = ABSZ(ALPHA)+ONEHAV                                           M32.8290
      NPOLES = NPOLES+JN                                                M32.8291
  460 CONTINUE                                                          M32.8292
C   == ORDER INCREMENT OR DECREMENT CHECK ==                            M32.8293
      IF (NPOLES.LE.MPOLES) GO TO 465                                   M32.8294
      IF (ITERLP.GT.1) GO TO 790                                        M32.8295
      GO TO 475                                                         M32.8296
  465 IF (NPOLES.LE.NORMAX) GO TO 470                                   M32.8297
      IF (ITERLP.GT.1) GO TO 795                                        M32.8298
      GO TO 475                                                         M32.8299
  470 D1 = NPOLE                                                        M32.8300
      D2 = NPOLES                                                       M32.8301
      IF (NPOLE.EQ.0) GO TO 485                                         M32.8302
      IF (D2/D1.LT.1.3D0) GO TO 480                                     M32.8303
      IF (ICHKP.EQ.1) GO TO 485                                         M32.8304
      TOLFAC = 1.1D0*TOLFAC                                             M32.8305
      ICHKP = 1                                                         M32.8306
      GO TO 265                                                         M32.8307
  475 TOLFAC = 2.5D0*TOLFAC                                             M32.8308
      INCRTL = INCRTL+1                                                 M32.8309
      IF (INCRTL.GT.10) GO TO 2160                                      M32.8310
      GO TO 265                                                         M32.8311
  480 IF (NPOLES.GT.NPOLE) GO TO 485                                    M32.8312
      IF (ICHKP.EQ.1) GO TO 482                                         M32.8313
      TOLFAC = .8D0*TOLFAC                                              M32.8314
      GO TO 265                                                         M32.8315
  482 TOLFAC = TOLFAC/1.1D0                                             M32.8316
      GO TO 265                                                         M32.8317
  485 ICHKP = 0                                                         M32.8318
C                                                                       M32.8319
      LRANGE = 0                                                        M32.8320
      DO 550 IRANGE = 1,NTOTRA                                          M32.8321
      ALPHA = ZOPRAO(4*IRANGE)                                          M32.8322
      IF (ALPHA.GT.0.) GO TO 490                                        M32.8323
      JN = -ALPHA+ONEHAV                                                M32.8324
      IF (JN.EQ.1) GO TO 540                                            M32.8325
      GO TO 500                                                         M32.8326
  490 JN = ALPHA+ONEHAV                                                 M32.8327
      IF (JN.EQ.1) GO TO 540                                            M32.8328
C   ASSIGN ZEROES FOR +ALPHA OR POLES FOR -ALPHA                        M32.8329
  500 PLOCPN = 0.                                                       M32.8330
      J2 = JN-1                                                         M32.8331
      DO 510 J = 1,J2                                                   M32.8332
      PLOCPN = PLOCPN+J                                                 M32.8333
  510 CONTINUE                                                          M32.8334
      ALOCPN = ZOPRAO(4*IRANGE-2)+DEMINP*PLOCPN/JN                      M32.8335
      LRANGE = LRANGE+(2*JN+2)                                          M32.8336
      AZEPO(LRANGE-2*JN-1) = ZOPRAO(4*IRANGE-3)                         M32.8337
      AZEPO(LRANGE-2*JN) = ALPHA                                        M32.8338
      AZEPO(LRANGE-JN) = ALOCPN                                         M32.8339
      DO 520 J = 1,J2                                                   M32.8340
      AZEPO(LRANGE-JN-J) = ALOCPN-J*DEMINP                              M32.8341
  520 CONTINUE                                                          M32.8342
C   ASSIGN POLES FOR +ALPHA OR ZEROES FOR -ALPHA                        M32.8343
      ALOCPN = ZOPRAO(4*IRANGE-1)-DEMINP*PLOCPN/JN                      M32.8344
      AZEPO(LRANGE-JN+1) = ALOCPN                                       M32.8345
      DO 530 J = 1,J2                                                   M32.8346
      AZEPO(LRANGE-JN+1+J) = ALOCPN+J*DEMINP                            M32.8347
  530 CONTINUE                                                          M32.8348
      GO TO 550                                                         M32.8349
  540 LRANGE = LRANGE+4                                                 M32.8350
      AZEPO(LRANGE-3) = ZOPRAO(4*IRANGE-3)                              M32.8351
      AZEPO(LRANGE-2) = ALPHA                                           M32.8352
      AZEPO(LRANGE-1) = ZOPRAO(4*IRANGE-2)                              M32.8353
      AZEPO(LRANGE)   = ZOPRAO(4*IRANGE-1)                              M32.8354
  550 CONTINUE                                                          M32.8355
      IF (IDEBUG.LT.2) GO TO 570                                        M32.8356
C   OUTPUT AZEPO VECTOR                                                 M32.8357
      WRITE(LUNIT6,7077)                                               
 7077 FORMAT(//,1X,4X, 5HXBEGR,3X, 5HALPHA,10X,                         
     142HPOLES AND ZEROES BEFORE ADJUSTMENT (LOG F),/)                  M33.2251
      INDEX = 0                                                         M32.8361
      DO 560 IRANGE = 1,NTOTRA                                          M32.8362
      ALPHA = AZEPO(INDEX+2)                                            M32.8363
      JN = ABSZ(ALPHA)+ONEHAV                                           M32.8364
      I1 = INDEX+1                                                      M32.8365
      I2 = I1+1+2*JN                                                    M32.8366
      WRITE(LUNIT6,7079)(AZEPO(I),I=I1,I2)                             
 7079 FORMAT(1X,E12.4,F4.0,6E11.4,/,17X,6E11.4,/,17X,6E11.4)            
      INDEX = I2                                                        M32.8369
  560 CONTINUE                                                          M32.8370
C   STORE INDEXES IN VECTOR                                             M32.8371
  570 INDXV(1) = 0                                                      M32.8372
      DO 580 I=2,NTOTRA                                                 M32.8373
      INDEX = INDXV(I-1)                                                M32.8374
      ALPHA = AZEPO(INDEX+2)                                            M32.8375
      JN = ABSZ(ALPHA)+ONEHAV                                           M32.8376
      INDXV(I) = INDEX+2+2*JN                                           M32.8377
  580 CONTINUE                                                          M32.8378
C  == CHECK FOR TYPE OF CURVE 2 ==                                      M32.8379
      ID = INDXV(NTOTRA)                                                M32.8380
      ALPHA = AZEPO(ID+2)                                               M32.8381
      IF (ICURVE.EQ.2.AND.ALPHA.GT.0. .AND. NFITMX .EQ. 0) IFTYPE = 1   M44. 801
C  CONVERT CORNERS TO FREQUENCY VALUES                                  M32.8383
      DO 600 IRANGE=1,NTOTRA                                            M32.8384
      INDEX = INDXV(IRANGE)                                             M32.8385
      ID = INDEX+2                                                      M32.8386
      ALPHA = AZEPO(ID)                                                 M32.8387
      JN = ABSZ(ALPHA)+ONEHAV                                           M32.8388
      J2N = 2*JN                                                        M32.8389
      D13 = 10.                                                         M32.8390
      DO 590 J=1,J2N                                                    M32.8391
  590 AZEPO(ID+J) = D13**AZEPO(ID+J) 
      IF ( IDEBUG .LT. 4 )  GO TO 600
      WRITE(LUNIT6,*) ' poles and zeroes in Hz b4 do 610 loop.'
      WRITE(LUNIT6,7078) (AZEPO(I),I=ID+1, ID+J2N)   ! THL
 7078 FORMAT ( (1X, 10E11.4) )
  600 CONTINUE                                                          M32.8393
C                                                                       M32.8394
C             CORNERS ADJUSTMENTS                                       M32.8395
C                                                                       M32.8396
      DO 610 J=1,3                                                      M32.8397
      IF (IDEBUG.GE.3)                                                  M32.8398
     1 WRITE(LUNIT6,7081) J                                            
 7081 FORMAT(//,10X,31H*** CORNERS ADJUSTMENT LOOP NO.,I2,3X, 3H***)    
      CALL ADJPK                                                        M32.8401
      CALL ADJCR                                                        M32.8402
      IF ( IDEBUG .LT. 4 )  GO TO 610   !THL
      WRITE(LUNIT6,*) ' AFTER CALL ADJCR, J=,', J  ! THL
      WRITE (LUNIT6, 7083)
      INDEX = 0                                                         M32.8409
      DO 375 IRANGE=1,NTOTRA                                            M32.8410
      ALPHA = AZEPO(INDEX+2)                                            M32.8411
      JN = ABSZ(ALPHA)+ONEHAV                                           M32.8412
      I1 = INDEX+1                                                      M32.8413
      I2 = I1+1+2*JN                                                    M32.8414
      WRITE(LUNIT6,7079) (AZEPO(I),I=I1,I2)                            
      INDEX = I2                                                        M32.8416
  375 CONTINUE               !  THL           
  610 CONTINUE                                                          M32.8403
      IF (IDEBUG.LT.3) GO TO 630      
C             OUTPUT POLES & ZEROES AFTER ADJUSTMENTS                   M32.8405
      WRITE(LUNIT6,7083)                                               
 7083 FORMAT(//,1X,4X, 5HXBEGR,3X,5HALPHA,10X,                          
     138HPOLES AND ZEROES AFTER ADJUSTMENT (HZ),/)                      M33.2254
      INDEX = 0                                                         M32.8409
      DO 620 IRANGE=1,NTOTRA                                            M32.8410
      ALPHA = AZEPO(INDEX+2)                                            M32.8411
      JN = ABSZ(ALPHA)+ONEHAV                                           M32.8412
      I1 = INDEX+1                                                      M32.8413
      I2 = I1+1+2*JN                                                    M32.8414
      WRITE(LUNIT6,7079) (AZEPO(I),I=I1,I2)                            
      INDEX = I2                                                        M32.8416
  620 CONTINUE                                                          M32.8417
C  ==  STORE ZEROES AND POLES ON SEPARATE VECTORS ==                    M32.8418
  630 NCORN = 0                                                         M32.8419
      DO 700 IRANGE=1,NTOTRA                                            M32.8420
      ID = INDXV(IRANGE)+2                                              M32.8421
      ALPHA = AZEPO(ID)                                                 M32.8422
      JN = ABSZ(ALPHA)+ONEHAV                                           M32.8423
      IF (ALPHA.GT.0.) GO TO 660                                        M32.8424
      DO 640 J=1,JN                                                     M32.8425
  640 FCP(NCORN+J) = AZEPO(ID+J)                                        M32.8426
      DO 650 J=1,JN                                                     M32.8427
  650 FCZ(NCORN+J) = AZEPO(ID+JN+J)                                     M32.8428
      GO TO 690                                                         M32.8429
  660 DO 670 J=1,JN                                                     M32.8430
  670 FCZ(NCORN+J) = AZEPO(ID+J)                                        M32.8431
      DO 680 J=1,JN                                                     M32.8432
  680 FCP(NCORN+J) = AZEPO(ID+JN+J)                                     M32.8433
  690 NCORN = NCORN+JN                                                  M32.8434
  700 CONTINUE                                                          M32.8435
      NPOLE = NCORN                                                     M32.8436
      NZERO = NCORN                                                     M32.8437
      IF (IFTYPE.EQ.2) NZERO = NZERO-JN                                 M32.8438
C                                                                       M32.8439
C              CHECK LEAST-SQUARES DEVIATION                            M32.8440
C                                                                       M32.8441
      ERROR1 = 0.                                                       M32.8442
      ERROR2 = 0.                                                       M32.8443
      X = XDAT(1)-DXCOMP                                                M32.8444
  710 X = X+DXCOMP                                                      M32.8445
      IF (X.GT.XDAT(NDATA)) GO TO 750                                   M32.8446
      FREQ = 10.D0**X                                                   M32.8447
      FREQ2 = FREQ**2                                                   M32.8448
      AMAGP = 1.D0                                                      M32.8449
      IF (NZERO.EQ.0) GO TO 725                                         M32.8450
      DO 720 J=1,NZERO                                                  M32.8451
  720 AMAGP = AMAGP*(FCZ(J)**2+FREQ2)/(FCP(J)**2+FREQ2)                 M32.8452
  725 AMAGLG = ALOG1Z(AMAGP)                                            M32.8453
      IF (IFTYPE.EQ.1) GO TO 740                                        M32.8454
      J1 = NZERO+1                                                      M32.8455
      DO 730 J=J1,NPOLE                                                 M32.8456
  730 AMAGLG = AMAGLG-ALOG1Z(FCP(J)**2+FREQ2)                           M32.8457
  740 AMAGLG = HREFLG+AMAGLG/2.D0                                       M32.8458
      D1 = AMAGLG-YFUN39(X)                                             M33.2255
      IF (X.GT.1.60D0.AND.X.LT.1.956D0) D1 = 3.D0*D1                    M32.8460
      ERROR1 = ERROR1+D1**2                                             M32.8461
      IF (X.LT.XBEGTL.OR.X.GT.XENDTL) GO TO 710                         M32.8462
      ERROR2 = ERROR2+D1**2                                             M32.8463
      GO TO 710                                                         M32.8464
  750 EPSER1 = ERROR1/NECHK1                                            M32.8465
      EPSER2 = ERROR2/NECHK2                                            M32.8466
      EPSERR = EPSER2                                                   M32.8467
      IF (IDEBUG.EQ.0) GO TO 756                                        M32.8468
      D1 = 100.D0*(10.D0**SQRTZ(EPSER1)-1.D0)                           M32.8469
      D2 = 100.D0*(10.D0**SQRTZ(EPSER2)-1.D0)                           M32.8470
      WRITE(LUNIT6,7085) D1,D2,NPOLE,NZERO                             
 7085 FORMAT(//,1X,19H*** LSQ AVE ERROR =,E12.4,1X, 1H%,7X,             
     1       17HLSQ CHECK ERROR =,E12.4,1X, 1H%,2X, 3H***,/,1X,         M33.2257
     2        3H***,6X,14HNO. OF POLES =,I3,20X,                        M33.2258
     3       15HNO. OF ZEROES =,I3,9X, 3H***,1X)                        M33.2259
      IF (IECODE.EQ.1) GO TO 754                                        M32.8476
      D1 = 100.D0*(10.D0**SQRTZ(EPSTOL)-1.D0)                           M32.8477
      WRITE(LUNIT6,7087) D1                                            
 7087 FORMAT(/,1X,46H--- ERROR CRITERION: LSQ CHECK ERROR LESS THAN,    
     1       F5.2,1X, 6H%  ---,1X)                                      M33.2261
      GO TO 756                                                         M32.8481
  754 WRITE(LUNIT6,7089) NORMAX                                        
 7089 FORMAT(/,1X,45H--- ERROR CRITERION: MINIMUM LSQ CHECK ERROR ,      
     1       28HWITHIN GIVEN MAXIMUM ORDER (,I3,2X,11HPOLES ) ---)      M33.2263
  756 IF (EPSERR.GE.ERROPT) GO TO 780  
C  STORE OPTIMUM VALUES                                                 M32.8486
      DO 760 J=1,NPOLE                                                  M32.8487
  760 FCPR(J) = FCP(J)                                                  M32.8488
      DO 770 J=1,NZERO                                                  M32.8489
  770 FCZR(J) = FCZ(J)                                                  M32.8490
      HRFLGR = HREFLG                                                   M32.8491
      NPOLER = NPOLE                                                    M32.8492
      NZEROR = NZERO                                                    M32.8493
      ERROPT = EPSERR                                                   M32.8494
  780 IF (EPSERR.LT.EPSTOL.AND.IECODE.EQ.0) GO TO 800                   M32.8495
      IF (EPSERR.GE.5.D0*ERROPT.AND.INELIM.EQ.0) GO TO 797
      TOLFAC = .8D0*TOLFAC                                              M32.8497
      GO TO 260                                                         M32.8498
  790 IF (IDEBUG.GE.1)                                                  M32.8499
     1 WRITE(LUNIT6,7091)                                              
 7091 FORMAT(//,1X,35H--- FURTHER ORDER INCREASE WAS NOT ,              
     143HPOSSIBLE BECAUSE, OF VECTORS DIMENSIONS ---,1X)                M33.2265
      GO TO 800                                                         M32.8503
  795 IF (IDEBUG.GE.1)                                                  M32.8504
     1 WRITE(LUNIT6,7093) NPOLES,NORMAX                                
 7093 FORMAT(//,1X,31H--- NO. OF POLES IN THIS LOOP (,I3,               
     134H) IS LARGER THAN SPECIFIED LIMIT (,I3, 5H) ---,1X)             M33.2267
      GO TO 800                                                         M32.8508
  797 IF (IDEBUG.GE.1)                                                  M32.8509
     1 WRITE(LUNIT6,7095)                                              
 7095 FORMAT(//,1X,                                                     
     1  55H--- NO FURTHER ORDER INCREASE WAS ALLOWED BECAUSE ERROR,/,1X,M37.7983
     2  54HIN THIS LOOP WAS 5 TIMES LARGER THAN PREVIOUS MINIMUM.,/,1X, M33.2269
     3  55HIF FURTHER ORDER INCREASE IS DESIRED MAKE "INELIM = 1" ,/,1X,M37.7984
     4  22HIN PARAMETERS LIST ---,1X)                                   M33.2271
  800 IF (ERROPT.GT.EPSTOL.AND.IDEBUG.GE.1)                             M32.8516
     1 WRITE(LUNIT6,7097)                                              
 7097 FORMAT(//,1X,42H--- MAXIMUM TOLERANCE CRITERION COULD NOT ,       
     1       10HBE MET ---)                                             M33.2273
C                                                                       M32.8520
C             TIME DELAY FOR PROPAGATION FUNCTION                       M32.8521
C                                                                       M32.8522
      IF (ICURVE .EQ. 1 .OR. NFITMX .EQ. 1)  GO TO 805                  M44. 802
      DX = .1D0                                                         M32.8524
      X = XDAT(1)-DX                                                    M32.8525
  900 X = X+DX                                                          M32.8526
      IF (X.GT.XDAT(NDATA)) GO TO 910                                   M32.8527
      IF (APH(X).LT.-.08727D0) GO TO 920                                M32.8528
      GO TO 900                                                         M32.8529
  920 X1 = X                                                            M32.8530
      X2 = XDAT(NDATA)                                                  M32.8531
      SUMTAU = 0.                                                       M32.8532
      N = 0                                                             M32.8533
      X = X1-DX                                                         M32.8534
  930 X = X+DX                                                          M32.8535
      IF (X.GT.X2) GO TO 940                                            M32.8536
      N = N+1                                                           M32.8537
      FREQ = 10.D0**X                                                   M32.8538
      PHP = 0.                                                          M32.8539
      DO 932 J=1,NZEROR                                                 M32.8540
  932 PHP = PHP+ATAN2Z(FREQ,FCZR(J))-ATAN2Z(FREQ,FCPR(J))               M32.8541
      IF (IFTYPE.EQ.1) GO TO 935                                        M32.8542
      J1 = NZEROR+1                                                     M32.8543
      DO 934 J=J1,NPOLER                                                M32.8544
  934 PHP = PHP-ATAN2Z(FREQ,FCPR(J))                                    M32.8545
  935 TAU = (PHP-APH(X))/FREQ                                           M32.8546
      SUMTAU = SUMTAU+TAU                                               M32.8547
      GO TO 930                                                         M32.8548
  940 TAUR = SUMTAU/(N*TWOPI)                                           M32.8549
      GO TO 950                                                         M32.8550
  910 WRITE(LUNIT6,7099)                                               
 7099 FORMAT(//,1X,                                                     
     160H*** SPECIFIED FREQUENCY RANGE DOES NOT CONTAIN ENOUGH CURVE ,  M33.2274
     2 /,57H DYNAMICS FOR AN ACCURATE PHASE DISPLACEMENT EVALUATION. ,  M33.2275
     3 /,57H TAU WILL BE TAKEN AS THE TRAVELLING TIME AT THE HIGHEST ,  M33.2276
     4 /,20H GIVEN FREQUENCY ***)                                       M33.2277
      TAUR = TRAVHF                                                     M32.8557
  950 IF (IDEBUG.EQ.0) GO TO 807                                        M35.8948
      D1 = 1.D3*TAUR                                                    M32.8559
      D2 = 10.D0**XDAT(NDATA)                                           M32.8560
      D3 = 1.D3*TRAVHF                                                  M32.8561
      WRITE(LUNIT6,8001) D1,D2,D3                                      
 8001 FORMAT(//,1X,24HPHASE DISPLACEMENT TAU =,E12.4,3X, 4HMSEC,        
     1 /,1X, 18HTRAVELLING TIME AT,E12.4,2X, 2HHZ,3X, 2HIS,             M33.2279
     2       E12.4,3X, 4HMSEC)                                          M33.2280
C   == ADD EXTRA POLE TO A1 FUNCTION WITH TYPE 1 FIT ==                 M32.8566
  807 IF (IFTYPE.EQ.2) GO TO 805                                        M35.8949
      NPOLER = NPOLER+1                                                 M32.8568
      IF (NPOLER.GT.MPOLES) GO TO 2150                                  M32.8569
      D1 = XDAT(NDATA)+5.D0                                             M32.8570
      FCPR(NPOLER) = 10.D0**D1                                          M32.8571
  805 IF (IFWTA.EQ.0) GO TO 850                                         M32.8572
C                                                                       M32.8573
C             COMPARISON TABLE                                          M32.8574
C                                                                       M32.8575
      D1 = 10.D0**HRFLGR                                                M32.8576
      WRITE(LUNIT6,8003) IFTYPE,D1                                     
 8003 FORMAT(/,1X,12HCURVE TYPE =,I2,4X,17HREFERENCE LEVEL =,E11.4)     
      WRITE(LUNIT6,8005) NPOLER,NZEROR                                 
 8005 FORMAT(/,1X,11HNO. POLES =,I3,4X,12HNO. ZEROES =,I3)              
      IF (ICURVE .EQ. 2  .OR. NFITMX .EQ. 1) GO TO 812                  M44. 803
      WRITE(LUNIT6,8007) IMODE                                         
 8007 FORMAT(//,30H TABLE OF Yc VS. YcEQ FOR MODE,I3,/,81H UNITS: FREQ. 
     1IN HZ, MAGNITUDE IN OHMS, PHASE IN DEG., DELMAG IN %, DELPH IN DEGM32.8584
     2.,//,5X,4HFREQ,6X,5HYcMAG,5X,7HYcEQMAG,5X,6HDELMAG,6X,4HYcPH,     M43.3071
     3          6X,6HYcEQPH,5X,5HDELPH,7X,4HFREQ)                       M43.3072
      GO TO 814                                                         M32.8587
  812 IF ( NFITMX .EQ. 1 )  GO TO 813                                   M44. 804
      WRITE(LUNIT6,8009) IMODE                                         
 8009 FORMAT ( /,1X,37HTABLE OF  "A1"  VS.  "A1EQ"  FOR MODE,I3,        
     1        /,1X,38HUNITS: FREQUENCY IN HERTZ,  MAGNITUDE ,           M33.2284
     2                40HIN PER UNIT,  PHASE IN DEGREES,  DELMAG ,      M33.2285
     3                24HIN %,  DELPH IN DEGREES., / , 1X,              M33.2286
     4          51HP1PH = A1EQPH+OMEGA*TAU; TAU IN MSEC; DELTAU IN % ., M33.2287
     5      //,5X,4HFREQ,6X,5HA1MAG,5X,7HA1EQMAG,5X,6HDELMAG,6X,4HA1PH, M32.8594
     6          6X,6HA1EQPH,5X,5HDELPH,7X,4HP1PH,7X,3HTAU,7X,6HDELTAU,  M32.8595
     7          6X,4HFREQ)                                              M32.8596
      GO TO 814                                                         M43.3075
  813 WRITE(LUNIT6,1813) ICURVE, IMODE                                  M44. 805
 1813 FORMAT(//,' TABLE OF Ti VS. TiEQ FOR ELEMENT ', 2I2, /,           M43.3077
     1 ' UNITS: FREQ. IN HZ, MAGNITUDE UNITLESS, PHASE IN DEG., DELMAG  M43.3078
     2 IN %, DELPH IN DEG.',//,5X,4HFREQ,6X,5HTiMAG,5X,7HTiEQMAG,5X,    M43.3079
     3 6HDELMAG,6X,4HTiPH, 6X,6HTiEQPH,5X,5HDELPH,7X,4HFREQ)            M43.3080
  814 DXCOMP = .1D0                                                     M32.8597
      X = XDAT(1)-DXCOMP                                                M32.8598
  810 X = X+DXCOMP                                                      M32.8599
      IF (X.GT.XDAT(NDATA)) GO TO 850                                   M32.8600
      FREQ = 10.D0**X                                                   M32.8601
      FREQ2 = FREQ**2                                                   M32.8602
      AMAGP = 1.D0                                                      M32.8603
      PHP = 0.                                                          M32.8604
      IF (NZEROR.EQ.0) GO TO 825                                        M32.8605
      DO 820 J=1,NZEROR                                                 M32.8606
      AMAGP = AMAGP*(FCZR(J)**2+FREQ2)/(FCPR(J)**2+FREQ2)               M32.8607
      PHP = PHP+ATAN2Z(FREQ,FCZR(J))-ATAN2Z(FREQ,FCPR(J))               M32.8608
  820 CONTINUE                                                          M32.8609
  825 AMAGLG = ALOG1Z(AMAGP)                                            M32.8610
      IF (IFTYPE.EQ.1) GO TO 840                                        M32.8611
      J1 = NZEROR+1                                                     M32.8612
      DO 830 J=J1,NPOLER                                                M32.8613
      AMAGLG = AMAGLG-ALOG1Z(FCPR(J)**2+FREQ2)                          M32.8614
      PHP = PHP-ATAN2Z(FREQ,FCPR(J))                                    M32.8615
  830 CONTINUE                                                          M32.8616
  840 AMAGLG = HRFLGR+AMAGLG/2.D0                                       M32.8617
      AMAGP = 10.D0**AMAGLG                                             M32.8618
      AX = 10.D0**YFUN39(X)                                             M33.2288
      D1 = (AMAGP/AX-1.D0)*100.D0                                       M32.8620
      D2 = APH(X)                                                       M32.8621
      D3 = D2*360.D0/TWOPI                                              M32.8622
      D4 = PHP*360.D0/TWOPI                                             M32.8623
      D5 = ABSZ(D4-D3)                                                  M32.8624
      IF (ICURVE.EQ.2 .AND. NFITMX .EQ. 0) GO TO 842                    M44. 806
      WRITE(LUNIT6,8011) FREQ,AX,AMAGP,D1,D3,D4,D5,FREQ                
 8011 FORMAT(1X,8E11.4)                                                 
      GO TO 810                                                         M32.8628
  842 TAU = (PHP-D2)/(TWOPI*FREQ)                                       M32.8629
      D8 = TAU*1.D3                                                     M32.8630
      D9 = (TAU/TAUR-1.D0)*100.D0                                       M32.8631
      D6 = D4-360.D0*FREQ*TAUR                                          M32.8632
      D7 = ABSZ(D6-D3)                                                  M32.8633
      WRITE(LUNIT6,8013) FREQ,AX,AMAGP,D1,D3,D6,D7,D4,D8,D9,FREQ       
 8013 FORMAT(1X,11E11.4)                                                
      GO TO 810                                                         M32.8636
C                                                                       M32.8637
  850 IF (IFPLOT.EQ.1)  CALL FTPLOT (ICURVE, IMODE, NFITMX)
C                                                                       M32.8640
C             PARTIAL FRACTION EXPANSION                                M32.8641
C                                                                       M32.8642
      DO 855 IFRAC=1,NPOLER                                             M32.8643
      S = -FCPR(IFRAC)                                                  M32.8644
      RESILG = HRFLGR                                                   M32.8645
      SIGN = 1.D0                                                       M32.8646
      IF (NZEROR.EQ.0) GO TO 875                                        M32.8647
      DO 860 J=1,NZEROR                                                 M32.8648
      IF (J.EQ.IFRAC) GO TO 865                                         M32.8649
      D1 = S+FCZR(J)                                                    M32.8650
      D2 = S+FCPR(J)                                                    M32.8651
      D3 = ABSZ(D1)                                                     M32.8652
      D4 = ABSZ(D2)                                                     M32.8653
      SIGN = SIGN*(D1/D3)*(D2/D4)                                       M32.8654
      RESILG = RESILG+ALOG1Z(D3)-ALOG1Z(D4)                             M32.8655
      GO TO 860                                                         M32.8656
  865 D1 = S+FCZR(J)                                                    M32.8657
      D3 = ABSZ(D1)                                                     M32.8658
      SIGN = SIGN*(D1/D3)                                               M32.8659
      RESILG = RESILG+ALOG1Z(D3)                                        M32.8660
  860 CONTINUE                                                          M32.8661
      IF (IFTYPE.EQ.1) GO TO 870                                        M32.8662
  875 J1 = NZEROR+1                                                     M32.8663
      DO 880 J=J1,NPOLER                                                M32.8664
      IF (J.EQ.IFRAC) GO TO 880                                         M32.8665
      D2 = S+FCPR(J)                                                    M32.8666
      D4 = ABSZ(D2)                                                     M32.8667
      SIGN = SIGN*(D2/D4)                                               M32.8668
      RESILG = RESILG-ALOG1Z(D4)                                        M32.8669
  880 CONTINUE                                                          M32.8670
  870 RESID = (10.D0**RESILG)*SIGN                                      M32.8671
  890 AKFRAC(IFRAC) = TWOPI*RESID                                       M32.8672
      IF ( MINUST(ICURVE) .EQ. 1 )                                      M43.3082
     1   AKFRAC(IFRAC) = - AKFRAC(IFRAC)                                M43.3083
      ALPHAF(IFRAC) = TWOPI*FCPR(IFRAC)                                 M32.8673
  855 CONTINUE                                                          M32.8674
C                                                                       M32.8675
C     IF (ICURVE.EQ.1) HREFR = 10.D0**HRFLGR                            M43.3084
      IF (ICURVE .EQ. 1 .OR. NFITMX .EQ. 1) HREFR = 10.D0**HRFLGR       M44. 808
C   == PUNCH PARAMETERS ==                                              M32.8678
      N23 = NPOLER                                                              
      IF ( IOFGND .EQ. 1 )                                                      
     1 N23 = ( NPOLER + 2 ) / 3 * 3                                             
      IF (ICURVE .EQ. 2 .AND. NFITMX .EQ. 0) GO TO 8779                 M44. 809
      IF (NFITMX .EQ. 1) GO TO 8794                                     M44. 810
      D6 = KOUTPR                                                       M32.8680
      N8 = 2 * IMODE                                                    M32.8681
      IF ( IMODAL  .GT.  0 .OR. MSPEDB .EQ. 1 )   GO TO  8787           M43.3088
      IF ( IPUNCH  .EQ.  0 )                                            M32.8683
     1 WRITE (LUNIT7, 8781) IMODE, VSTACS(N8-1), VSTACS(N8), D6         M32.8684
 8781 FORMAT ( 1H-,  I1,  2A6,  12X,  F6.0,4X, 2H1.,14X, 2H-4,          M43.3089
     1 3X, 1H1 )                                                        M43.3090
      IF (IDEBUG.EQ.0) GO TO 8794                                       M32.8686
      WRITE (LUNIT1, 8782) IMODE, VSTACS(N8-1), VSTACS(N8), D6          M32.8687
 8782 FORMAT ( 1X,  1H-,  I1,  2A6,  12X,  F6.0, 4X, 2H1.,14X,          M43.3091
     1 2H-4,3X,1H1,24H                                       )          M43.3092
      KOUNT = KOUNT+1                                                   M32.8689
      GO TO 8794                                                        M32.8690
 8787 IF ( ITRNSF .EQ. 1 )  IFQDPT = 0                                          
      IF ( IPUNCH  .EQ.  0 )                                            M43.3094
     1 WRITE (LUNIT7, 8790)  IMODE, VSTACS(N8-1), VSTACS(N8),           M32.8692
     2                       D6, NMODE, IFQDPT                          M43.3095
 8790 FORMAT ( 1H-, I1, 2A6, 12X, F6.0,4X, 2H1.,14X, 2H-4,              M43.3096
     1 2I2)                                                             M43.3097
      IF (IDEBUG.EQ.0) GO TO 8794                                       M32.8695
      WRITE (LUNIT1, 8791)  IMODE, VSTACS(N8-1), VSTACS(N8),            M32.8696
     1                      D6, NMODE, IFQDPT                           M43.3098
 8791 FORMAT ( 1X,1H-,I1,2A6, 12X, F6.0,4X,2H1.,14X, 2H-4,2I2,          M43.3099
     1        24H                                        )              M43.3100
      KOUNT = KOUNT+1                                                   M32.8699
      GO TO 8794                                                        M43.3101
 8793 WRITE (LUNIT6, *) ' This is a near-zero Tij element.'             M43.3102
      NPOLER = 1                                                        M43.3103
      HREFR  = 0.                                                       M43.3104
      AKFRAC(1) = 0.0                                                   M43.3105
      ALPHAF(1) = 1.0                                                   M43.3106
      N23 = 1                                                           M43.3107
      IF ( IOFGND .EQ. 1 )  N23 = 3                                     M43.3108
      GO TO 8794                                                        M43.3109
 8792 NPOLER = 1                                                        M43.3110
      HREFR = 1.                                                        M43.3111
      AKFRAC(1) = 0.0                                                   M43.3112
      ALPHAF(1) = 1.0                                                   M43.3113
      NUMONE = 0                                                        M43.3114
      N23 = 1                                                           M43.3115
      IF ( IOFGND .EQ. 1 )  N23 = 3                                     M43.3116
 8794 IF ( MINUST(ICURVE) .EQ. 1 )  HREFR = - HREFR                     M43.3117
      IF ( IPUNCH  .EQ.  0 )                                            M43.3118
     1 WRITE (LUNIT7, 8798)  NPOLER,HREFR                               M32.8701
 8798 FORMAT ( I8, E32.20 )                                             M32.8702
      IF (IDEBUG.EQ.0) GO TO 8788                                       M32.8703
      WRITE (LUNIT1, 8799)  NPOLER,HREFR                                M32.8704
 8799 FORMAT ( 1X,  I8,  E32.20,                                        M39.3331
     1     42H                                              )           M39.3332
      KOUNT = KOUNT+1                                                   M32.8706
      GO TO 8788                                                        M32.8707
 8779 IF ( IPUNCH  .EQ.  0 )                                            M32.8708
     1 WRITE (LUNIT7, 8798)  NPOLER,TAUR                                M32.8709
      IF (IDEBUG.EQ.0) GO TO 8788                                       M32.8710
      WRITE (LUNIT1, 8799)  NPOLER,TAUR                                 M32.8711
      KOUNT = KOUNT+1                                                   M32.8712
 8788 IF ( IPUNCH  .EQ.  0 )                                            M32.8713
     1 WRITE (LUNIT7, 8803)  ( AKFRAC(KP), KP=1,NPOLER )                M32.8714
 8803 FORMAT ( 3E26.18 )                                                M32.8715
      IF (IDEBUG.EQ.0) GO TO 8806                                       M32.8716
      WRITE (LUNIT1, 8804)  ( AKFRAC(KP), KP=1, N23 )                   M39.3333
 8804 FORMAT ( 1X,  3E26.18,  4H      )                                 M39.3334
 8806 IF ( IPUNCH  .EQ.  0 )                                            M32.8719
     1 WRITE (LUNIT7, 8803)  ( ALPHAF(KP), KP=1,NPOLER )                M32.8720
      IF (IDEBUG.EQ.0) GO TO 8810                                       M43.3119
      WRITE (LUNIT1, 8804)  ( ALPHAF(KP), KP=1, N23 )                   M39.3335
      D1 = NPOLER/3.D0                                                  M32.8723
      N2 = NPOLER/3                                                     M32.8724
      KOUNT = KOUNT+2*N2                                                M32.8725
      IF (D1.GT.N2) KOUNT = KOUNT+2                                     M32.8726
 8810 IF ( IMODE  .LT. 2 )  GO TO 3200                                  M43.3120
      IF ( IPRSUP .GE. 1)                                               M43.3121
     1 write (LUNIT6,*) ' imode , imodal, and mspedb at 8810 =',        M43.3122
     1        imode, imodal, mspedb                                     M43.3123
      IF ( IMODAL .GT. 0 )  GO TO 3200                                  M43.3124
      IF ( MSPEDB .EQ. 1  .AND.  IMODE .EQ. 2 )  GO TO 3200             M43.3125
      IF ( IPRSUP .GE. 1 )                                              M43.3126
     1 write (LUNIT6,*) ' icurve =', icurve                             M43.3127
      IF ( ICURVE .EQ. 2 .AND. NFITMX .EQ. 0 )  GO TO 8815              M44. 811
      NPOLS1 = NPOLER                                                   M43.3129
      IF ( IPRSUP .GE. 1 )                                              M43.3130
     1  write (LUNIT6,*) ' Storing  Zc info.'                           M43.3131
      DO 8812  KP = 1, NPOLER                                           M43.3132
      AKFRS1(KP) = AKFRAC(KP)                                           M43.3133
      ALPHS1(KP) = ALPHAF(KP)                                           M43.3134
 8812 CONTINUE                                                          M43.3135
      GO TO 3200                                                        M43.3136
 8815 NPOLS2 = NPOLER                                                   M43.3137
      IF ( IPRSUP .GE. 1 )                                              M43.3138
     1  write (LUNIT6,*) ' Storing A1 info.'                            M43.3139
      DO 8816  KP = 1, NPOLER                                           M43.3140
      AKFRS2(KP) = AKFRAC(KP)                                           M43.3141
      ALPHS2(KP) = ALPHAF(KP)                                           M43.3142
 8816 CONTINUE                                                          M43.3143
      GO TO 6000                                                        M43.3144
C                                                                       M32.8727
 3200 CONTINUE                                                          M32.8728
C                                                                       M32.8729
 3300 CONTINUE                                                          M32.8730
      if ( itrnsf .NE. 1 )  go to 8820                                          
      if ( NFITMX .ne. 0 )  go to 8841                                          
      NFITMX = 1                                                                
      go to 8777                                                                
 6000 DO 6300  IMODE = 3, NMODE                                         M43.3147
      DO 6280  ICURVE = 1, 2                                            M43.3148
      IF ( IPRSUP .GE. 1)                                               M43.3149
     1  write (LUNIT6,*) ' restoring the stored info at 6300.'          M43.3150
      IF ( MSPEDB .EQ. 1  .AND.  IMODE .EQ. 3 )  GO TO 6300             M43.3151
      IF ( ICURVE .EQ. 1 )  NPOLER = NPOLS1                             M43.3152
      IF ( ICURVE .EQ. 2 )  NPOLER = NPOLS2                             M43.3153
      N23 = NPOLER                                                      M43.3154
      IF ( IOFGND .EQ. 1 )                                                      
     1 N23 = ( NPOLER + 2 ) / 3 * 3                                             
      IF (ICURVE.EQ.2) GO TO 6779                                       M43.3157
      D6 = KOUTPR                                                       M43.3158
      N8 = 2 * IMODE                                                    M43.3159
      IF ( MSPEDB  .GT.  0 )   GO TO  6787                              M43.3160
      IF ( IPUNCH  .EQ.  0 )                                            M43.3161
     1 WRITE (LUNIT7, 8781) IMODE, VSTACS(N8-1), VSTACS(N8), D6         M43.3162
      IF (IDEBUG.EQ.0) GO TO 6794                                       M43.3163
      WRITE (LUNIT1, 8782) IMODE, VSTACS(N8-1), VSTACS(N8), D6          M43.3164
      KOUNT = KOUNT+1                                                   M43.3165
      GO TO 6794                                                        M43.3166
 6787 IF ( IPUNCH  .EQ.  0 )                                            M43.3167
     1 WRITE (LUNIT7, 8790)  IMODE, VSTACS(N8-1), VSTACS(N8),           M43.3168
     2                       D6, NMODE, IFQDPT                          M44. 816
      IF (IDEBUG.EQ.0) GO TO 6794                                       M43.3170
      WRITE (LUNIT1, 8791)  IMODE, VSTACS(N8-1), VSTACS(N8),            M43.3171
     1                      D6, NMODE, IFQDPT                           M44. 817
      KOUNT = KOUNT+1                                                   M43.3173
 6794 IF ( IPUNCH  .EQ.  0 )                                            M43.3174
     1 WRITE (LUNIT7, 8798)  NPOLER,HREFR                               M43.3175
      IF (IDEBUG.EQ.0) GO TO 6788                                       M43.3176
      WRITE (LUNIT1, 8799)  NPOLER,HREFR                                M43.3177
      KOUNT = KOUNT+1                                                   M43.3178
      GO TO 6788                                                        M43.3179
 6779 IF ( IPUNCH  .EQ.  0 )                                            M43.3180
     1 WRITE (LUNIT7, 8798)  NPOLER,TAUR                                M43.3181
      IF (IDEBUG.EQ.0) GO TO 6788                                       M43.3182
      WRITE (LUNIT1, 8799)  NPOLER,TAUR                                 M43.3183
      KOUNT = KOUNT+1                                                   M43.3184
 6788 IF ( ICURVE .EQ. 2 )  GO TO 6800                                  M43.3185
      IF ( IPUNCH  .EQ.  0 )                                            M43.3186
     1 WRITE (LUNIT7, 8803)  ( AKFRS1(KP), KP=1,NPOLER )                M43.3187
      IF (IDEBUG.EQ.0) GO TO 6806                                       M43.3188
      WRITE (LUNIT1, 8804)  ( AKFRS1(KP), KP=1, N23 )                   M43.3189
 6806 IF ( IPUNCH  .EQ.  0 )                                            M43.3190
     1 WRITE (LUNIT7, 8803)  ( ALPHS1(KP), KP=1,NPOLER )                M43.3191
      IF (IDEBUG.EQ.0) GO TO 6280                                       M43.3192
      WRITE (LUNIT1, 8804)  ( ALPHS1(KP), KP=1, N23 )                   M43.3193
      GO TO 6810                                                        M43.3194
 6800 IF ( IPUNCH  .EQ.  0 )                                            M43.3195
     1 WRITE (LUNIT7, 8803)  ( AKFRS2(KP), KP=1,NPOLER )                M43.3196
      IF (IDEBUG.EQ.0) GO TO 6807                                       M43.3197
      WRITE (LUNIT1, 8804)  ( AKFRS2(KP), KP=1, N23 )                   M43.3198
 6807 IF ( IPUNCH  .EQ.  0 )                                            M43.3199
     1 WRITE (LUNIT7, 8803)  ( ALPHS2(KP), KP=1,NPOLER )                M43.3200
      IF (IDEBUG.EQ.0) GO TO 6280                                       M43.3201
      WRITE (LUNIT1, 8804)  ( ALPHS2(KP), KP=1, N23 )                   M43.3202
 6810 D1 = NPOLER/3.D0                                                  M43.3203
      N2 = NPOLER/3                                                     M43.3204
      KOUNT = KOUNT+2*N2                                                M43.3205
      IF (D1.GT.N2) KOUNT = KOUNT+2                                     M43.3206
 6280 CONTINUE                                                          M43.3207
 6300 CONTINUE                                                          M43.3208
C                                                                       M32.8731
C   == PUNCH TRANSFORMATION MATRIX ==                                   M32.8732
 8820 IF ( IMODAL  .EQ.  0   .AND.  MSPEDB .EQ. 0 )   GO TO 8841        M43.3209
      IF ( IMODAL .GT. 0 )  GO TO 8822                                  M43.3210
 8015 D1 = 2.0                                                          
      D2 = 6.0                                                          M43.3212
      D3 = 1.0/SQRTZ(D1)                                                M43.3213
      D4 = 1.0/SQRTZ(D2)                                                M43.3214
      N1 = 1                                                            M43.3215
      DO 8017  I = 1, 6                                                        
      DO 8019  J = 1, 6                                                        
      IF ( I .GT. 1 )  GO TO 8021                                      
      TIR(J,I) = D4                                                     M43.3219
      GO TO 8023                                                       
 8021 IF ( I .GT. 2 )  GO TO 8025                                      
      IF ( J .GT. 3 )  GO TO 8027                                      
      TIR(J,I) =  D4                                                    M43.3223
      GO TO 8023                                                       
 8027 TIR(J,I) = -D4                                                    
      GO TO 8023                                                        
 8025 IF ( I .GT. 3 )  GO TO 8029                                      
      IF ( J .GT. 2 )  GO TO 8031                                      
      IF ( J .EQ. 1 )  TIR(J,I) =  D3                                   M43.3229
      IF ( J .EQ. 2 )  TIR(J,I) = -D3                                   M43.3230
      GO TO 8023                                                       
 8031 TIR(J,I) = 0.                                                     
      GO TO 8023                                                       
 8029 IF ( I .GT. 4 )  GO TO 8033                                      
      IF ( J .GT. 3 )  GO TO 8035                                      
      IF ( J .GT. 2 )  GO TO 8037                                      
      TIR(J,I) = D4                                                     M43.3237
      GO TO 8023                                                       
 8037 TIR(J,I) = - D1*D4                                                
      GO TO 8023                                                       
 8035 TIR(J,I) = 0.                                                     
      GO TO 8023                                                       
 8033 IF ( I .GT. 5 )  GO TO 8039                                      
      IF ( J .GE. 4 .AND. J .LE. 5) GO TO 8041                        
      GO TO 8031                                                       
 8041 IF ( J .EQ. 4 )  TIR(J,I) =   D3                                  
      IF ( J .EQ. 5 )  TIR(J,I) = - D3                                  M43.3247
      GO TO 8023                                                       
 8039 IF ( J .GT. 3)  GO TO 8043                                       
      GO TO 8031                                                       
 8043 IF ( J .GT. 5 )  GO TO 8045                                      
      TIR(J,I) = D4                                                     M43.3252
      GO TO 8023                                                       
 8045 TIR(J,I) = - D1 * D4                                              
 8023 TII(J,I) = 0.0                                                    
 8019 CONTINUE                                                          
 8017 CONTINUE                                                          
 8047 IF (IPRSUP .GE. 1 )                                               
     1  WRITE (LUNIT6, 8049)   ((TIR(J,I), I=1, 6),J=1,6),             
     1                          ((TII(J,I), I=1, 6),J=1,6)              M43.3260
 8049 FORMAT ( ' TIR and TII for this special transposed double circuit 
     1 are', /,    (1X, 8E15.7) )                                       M43.3262
 8822 NPOLER = 1                                                        M43.3263
      AKFRAC(1) = 0.0                                                   M43.3264
      ALPHAF(1) = 1.0                                                   M43.3265
      N23 = 1                                                           M43.3266
      IF ( IOFGND .EQ. 1 )                                                      
     1 N23 = ( NPOLER + 2 ) / 3 * 3                                             
      DO 8832  IMODE=1, NMODE                                                   
      DO 8834  J = 1, NMODE                                                     
      HREFR = TIR (J, IMODE )                                           M43.3271
      IF ( IPUNCH  .EQ.  0 )                                            M43.3272
     1 WRITE (LUNIT7, 8798)  NPOLER,HREFR                               M43.3273
      IF (IDEBUG.EQ.0) GO TO 8836                                       M43.3274
      WRITE (LUNIT1, 8799)  NPOLER,HREFR                                M43.3275
      KOUNT = KOUNT+1                                                   M43.3276
 8836 IF ( IPUNCH  .EQ.  0 )                                            M43.3277
     1 WRITE (LUNIT7, 8803)   AKFRAC(1)                                 M43.3278
      IF (IDEBUG.EQ.0) GO TO 8838                                       M43.3279
      WRITE (LUNIT1, 8804)   AKFRAC(1)                                  M43.3280
 8838 IF ( IPUNCH  .EQ.  0 )                                            M43.3281
     1 WRITE (LUNIT7, 8803)   ALPHAF(1)                                 M43.3282
      IF (IDEBUG.EQ.0) GO TO 8834                                       M44.2304
      WRITE (LUNIT1, 8804)   ALPHAF(1)                                  M43.3284
      KOUNT = KOUNT+2                                                   M43.3285
 8834 CONTINUE                                                          M43.3286
 8832 CONTINUE                                                          M32.8748
C                                                                       M32.8749
 8841 IF ( IDEBUG  .GE.  1 )                                            M32.8750
     1 WRITE (LUNIT6, 8768)                                             M32.8751
 8768 FORMAT ( /, 39H PUNCHED OUTPUT (ON LUNIT7) BEGINS WITH,           M32.8752
     1            30H COMMENT CARDS DOCUMENTING THE,                    M32.8753
     2            31H TRANSMISSION CIRCUIT GEOMETRY.    ,/,             M32.8754
     3            33H THE FOLLOWING IS A RECORD OF THE,                 M32.8755
     4            35H PUNCHING,  INCLUDING THE FINAL TWO,               M32.8756
     5            34H COLUMNS WHICH DO NOT FIT ON CARD.  ,/,            M32.8757
     6                            1X,  82( 1H= )  ,/,                   M32.8758
     7            43H 001234567890123456789012345678901234567890,       M32.8759
     8            40H1234567890123456789012345678901234567890           M32.8760
     9                  ,/,    1X,  82( 1H= )    )                      M32.8761
C   == RECOVER INFORMATION FROM LUNIT1, PRINT ON LUNIT6 ==              M32.8762
      REWIND LUNIT1                                                     M32.8763
      IF ( KOUNT .EQ. 0 )  GO TO 1550                                   M35.8950
      DO 8835 J=1,KOUNT                                                 M32.8764
      READ(LUNIT1,1542) (TEXTP(I), I=1,14)                              M32.8765
 1542 FORMAT(13A6,A2)                                                   M32.8766
      WRITE(LUNIT6,1542) (TEXTP(I), I=1,14)                             M32.8767
 8835 CONTINUE                                                          M32.8768
      GO TO 1550                                                        M32.8769
 9200 LSTAT(18)=NCHAIN                                                  M32.8770
      LASTOV=NCHAIN                                                     M32.8771
      NCHAIN=51                                                         M32.8772
 9999 IF ( IPRSUP  .GE.  6 )                                            M32.8773
     1 WRITE (LUNIT6, 9998)  NCHAIN, KILL                               M32.8774
 9998 FORMAT (  32H EXIT "SUBR39".   NCHAIN, KILL =,  2I6  )            M32.8775
      RETURN                                                            M32.8776
C                                                                       M32.8777
C             TERMINATING CONDITIONS                                    M32.8778
C                                                                       M32.8779
 2170 WRITE(LUNIT6,8050) NMODE,MMODES                                  
 8050 FORMAT(//,1X,21H%%% NUMBER OF MODES (,I2,17H) EXCEEDS LIMIT (,    
     1       I2,27H). EXECUTION TERMINATED %%%)                         M33.2290
      CALL STOPTP                                                               
 2100 WRITE(LUNIT6,8051) MDAPTS                                        
 8051 FORMAT(//,1X,31H%%% DIMENSION OF DATA VECTORS (,I3,               
     1       36H) EXCEEDED. EXECUTION TERMINATED %%%)                   M33.2292
      CALL STOPTP                                                               
 2110 WRITE(LUNIT6,8053) MXKNEE                                        
 8053 FORMAT(//,1X,24H%%% DIMENSION OF XKNEE (,I2,                      
     1       36H) EXCEEDED. EXECUTION TERMINATED %%%)                   M33.2294
      CALL STOPTP                                                               
 2120 WRITE(LUNIT6,8055) MXCHKR                                        
 8055 FORMAT(//,1X,25H%%% DIMENSION OF XCHKRA (,I3,                     
     1       36H) EXCEEDED. EXECUTION TERMINATED %%%)                   M33.2296
      CALL STOPTP                                                               
 2130 WRITE(LUNIT6,8057) MZOPRA                                        
 8057 FORMAT(//,1X,25H%%% DIMENSION OF ZOPRAU (,I3,                     M33.2297
     1       36H) EXCEEDED. EXECUTION TERMINATED %%%)                   M33.2298
      CALL STOPTP                                                               
 2140 WRITE(LUNIT6,8059) MZOPRA                                        
 8059 FORMAT(//,1X,25H%%% DIMENSION OF ZOPRAO (,I3,                     
     1       36H) EXCEEDED. EXECUTION TERMINATED %%%)                   M33.2300
      CALL STOPTP                                                               
 2160 WRITE(LUNIT6,8061) NORMAX                                        
 8061 FORMAT(//,1X,39H%%% THE ALGORITHM CANNOT FIT THIS CURVE,          
     1    37H WITHIN THE MAXIMUM ORDER SPECIFIED (,I3,                  M33.2302
     2     2H).,/,1X,28H=== INCREASE THIS LIMIT ===.,/,1X,              M33.2303
     3    34HNO APPROXIMATION WAS GENERATED %%%)                        M33.2304
      CALL STOPTP                                                               
 2150 WRITE(LUNIT6,8063) MPOLES                                        
 8063 FORMAT(//,1X,33H%%% DIMENSION OF CORNER VECTORS (,I2,             
     1       36H) EXCEEDED. EXECUTION TERMINATED %%%)                   M33.2306
      CALL STOPTP                                                               
      RETURN                                                                    
      END                                                               M32.8814
      SUBROUTINE LOCSL (XBEG,XEND,ALPHA,XMID,XCORNA,XCORNB,ERYMAX)      M32.8815
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL39.FOR;/NOLIST'                                           
      INCLUDE  'DECK39.FOR;/NOLIST'                                           
      D78 = XEND - XBEG
      IF ( ABSZ(D78) .GE.  0.01 )  GO TO 50
      IF ( D78 .GT. 0.0 )  XMID = XBEG + D78/2  ! REALLY SMALL RANGE
      IF ( D78 .LT. 0.0 )  XMID = XEND - D78/2  ! CONSIDERATION,  THL
      GO TO 80                                     !  9/14/89
   50 CALL SPLIT (XBEG,XEND, ONEHAV, XMID)                             
   80 D1 = ABSZ( XEND - XMID)                                          
      IF ( D1 .LE. 0.001D0)  RETURN                                             
      IF ( IDEBUG .LT. 4 )  GO TO 2100                                  VAX.3418
      REFD2 = ABSZ ( (REFA-REFB) * ONEHAV )                             M32.8821
      REFDPC = 100.D0*(10.D0**REFD2-1.D0)                               M32.8822
      WRITE(LOUT,8065) REFA,REFB,REFDPC                                
 8065 FORMAT(1X, 5HREFA=,E11.4,3X, 5HREFB=,E11.4,3X, 7HREFDPC=,         
     1E11.4, 1H%  )                                                     M33.2310
C   PLACE SEGMENT INITIALLY AT MID POINT                                M32.8826
 2100 D2 = (REFB-REFA)/2./ALPHA                                         VAX.3421
      XCORNA = XMID - D2                                                VAX.3422
      XCORNB = XMID + D2                                                VAX.3423
      JN = ABSZ(ALPHA)                                                  M32.8829
      D1 = XEND-XBEG                                                    M32.8830
      XDELTA = D1/(20*JN)                                               M32.8831
      DXEDMA = D1/(50*JN)                                               M32.8832
C                                                                       M32.8833
      DO 260 ITER=1,2                                                   M32.8834
      XCORAR = XCORNA                                                   M32.8835
      XCORBR = XCORNB                                                   M32.8836
      XCORNA = XCORNA-XDELTA                                            M32.8837
      XCORNB = XCORNB-XDELTA                                            M32.8838
      ERROPT = 1.D12                                                    M32.8840
      IDEFAC = -1                                                       M32.8841
      LSIGN = 1
C    In 248 loop, N=1 is for R-shift, N=2 is for L-shift
      DO 248  N = 1, 2
      IF ( N .EQ. 1 )  GO TO 98
      IDEFAC = 0
      LSIGN = -1
   98 DO 160 J = 1,5                                                    
      IDEFAC = IDEFAC+LSIGN
      XCORNA = XCORNA+XDELTA*LSIGN
      XCORNB = XCORNB+XDELTA*LSIGN
C   CHECK ALLOCATION ERROR                                              M32.8846
      ERROR = 0.                                                        M32.8847
      X = XBEG-XDELTA                                                   M32.8848
  100 X = X+XDELTA                                                      M32.8849
      IF (X.GT.XEND) GO TO 140                                          M32.8850
      IF (X.LE.XCORNA) GO TO 110                                        M32.8851
      IF (X.GT.XCORNB) GO TO 120                                        M32.8852
      YAPPRX = REFA+(X-XCORNA)*ALPHA                                    M32.8853
      GO TO 130                                                         M32.8854
  110 YAPPRX = REFA                                                     M32.8855
      GO TO 130                                                         M32.8856
  120 YAPPRX = REFB                                                     M32.8857
  130 ERROR = ERROR+(YAPPRX-YFUN39(X))**2                               M33.2311
      GO TO 100                                                         M32.8859
  140 IF (IDEBUG.LT.4)  GO TO 8069
      IF ( N .EQ. 1 )
     1 WRITE(LOUT,8067) IDEFAC,XCORNA,XCORNB,ERROR,ERROPT              
 8067 FORMAT(1X, 7HSHIFT R,I5,4E12.4)                                   
      IF ( N .EQ. 2 )
     1 WRITE(LOUT,8068) IDEFAC, XCORNA, XCORNB, ERROR, ERROPT
 8068 FORMAT(1X, 7HSHIFT L, I5, 4E12.4)
 8069 IF(ERROR.GE.ERROPT) GO TO 150    ! Avoid infinite loop, change
      ERROPT = ERROR                   ! .GT. to .GE.,  THL, 9/12/89
      IDEFOP = IDEFAC                                                   M32.8865
  150 IF (ERROR.GE.2./ITER*ERROPT) GO TO 170   !  THL
  160 CONTINUE                                                          M32.8867
  170 IF ( N .EQ. 2 )  GO TO 250
C   RESTORE ORIGINAL POSITION BEFORE SHIFTING TO THE LEFT               M32.8868
      XCORNA = XCORAR                                                   
      XCORNB = XCORBR                                                   M32.8870
  248 CONTINUE
C   SHIFT ACCORDING TO OPTIMUM VALUE                                    M32.8899
  250 XCORNA = XCORAR+IDEFOP*XDELTA                                     M32.8900
      XCORNB = XCORBR+IDEFOP*XDELTA                                     M32.8901
      XDELTA = XDELTA/4.                                                M32.8902
  260 CONTINUE                                                          M32.8903
C  OBTAIN MAXIMUM DEVIATION                                             M32.8904
      ERYMAX = 0.                                                       M32.8905
      XL1 = XKNEE(IZONE)                                                M32.8906
      XL2 = XKNEE(IZONE+1)                                              M32.8907
      X = XCORNA-DXEDMA                                                 M32.8908
  270 X = X+DXEDMA                                                      M32.8909
      IF (X.GT.XCORNB) GO TO 310                                        M32.8910
      X1 = X-DXEDMA                                                     M32.8911
      X2 = X+DXEDMA                                                     M33.2315
      IF (X1.LT.XL1) X1 = X                                             M32.8913
      IF (X2.GT.XL2) X2 = X                                             M32.8914
      IF (X1.NE.X2) GO TO 280                                           M32.8915
      AGAMMA = 0.                                                       M32.8916
      GO TO 290                                                         M32.8917
  280 AGAMMA = (YFUN39(X2)-YFUN39(X1))/(X2-X1)                          M33.2316
  290 YAPPRX = REFA+(X-XCORNA)*ALPHA                                    M32.8919
      D13 = 1.0 + AGAMMA**2                                             M32.8920
      ERRORY = ABSZ ( (YAPPRX-YFUN39(X)) / SQRTZ ( D13 )  )             M33.2317
      IF (ERRORY.LT.ERYMAX) GO TO 300                                   M32.8922
      ERYMAX = ERRORY                                                   M32.8923
  300 GO TO 270                                                         M32.8924
  310 RETURN                                                            M32.8925
      END                                                               M32.8926
      SUBROUTINE ADJPK                                                  M32.8927
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION FPZ1(50),FPZ2(50)                                       VAX.3424
      INCLUDE  'LABL39.FOR;/NOLIST'                                           
      INCLUDE  'DECK39.FOR;/NOLIST'                                           
C  CHANGE THIS LIMIT IF DIMENSION IS CHANGED                            M32.8931
C   ORDER OF SLOPE IN SEGMENT (CONSECUTIVE POLES OR ZEROES)             M32.8932
      MDIMPZ = 50                                                       VAX.3425
      IF (NTOTRA.EQ.1) GO TO 340                                        M32.8934
      NTOTR1 = NTOTRA-1                                                 M32.8935
      DO 330 IRANGE = 1,NTOTR1                                          M32.8936
      INDEX1 = INDXV(IRANGE)                                            M32.8937
      INDEX2 = INDXV(IRANGE+1)                                          M32.8938
      ALPH1 = AZEPO(INDEX1+2)                                           M32.8939
      ALPH2 = AZEPO(INDEX2+2)                                           M32.8940
      IF (ALPH1*ALPH2.GT.0.) GO TO 330                                  M32.8941
      XL1 = AZEPO(INDEX1+1)                                             M32.8942
      IF (IRANGE.LT.NTOTR1) GO TO 100                                   M32.8943
      XR1 = XDAT(NDATA)                                                 M32.8944
      GO TO 110                                                         M32.8945
  100 ID = INDXV(IRANGE+2)                                              M32.8946
      XR1 = AZEPO(ID+1)                                                 M32.8947
  110 XPEAK = AZEPO(INDEX2+1)                                           M32.8948
      REFA = YFUN39(XPEAK)                                              VAX.3426
      REFB = YFUN39(XL1)                                                VAX.3427
      CALL SPLIT (XPEAK,XL1, ONEQTR, XBEG)                              M32.8949
      REFB = YFUN39(XR1)                                                VAX.3428
      CALL SPLIT (XPEAK,XR1, ONEQTR, XEND)                              M32.8950
      JN1 = ABSZ(ALPH1)+ONEHAV                                          M32.8951
      JN2 = ABSZ(ALPH2)+ONEHAV                                          M32.8952
      IF (JN1.GT.MDIMPZ.OR.JN2.GT.MDIMPZ) GO TO 2100                    M32.8953
      ID1 = INDEX1+2+JN1                                                M32.8954
      ID2 = INDEX2+2                                                    M32.8955
      FLIM1 = AZEPO(ID1)                                                M32.8956
      FLIM2 = AZEPO(ID2+JN2+1)                                          M32.8957
      IF (IDEBUG.GE.3)                                                  M32.8958
     1 WRITE(LOUT,8071) XPEAK                                          
 8071 FORMAT(/,1X,10X,19HADJUSTMENT OF PEAK:,E12.4)                     
      DJN1 = 1.D0 / JN1                                                 M39.3343
      DJN2 = 1.D0 / JN2                                                 M39.3344
      ERROPT = 1.E12   !  THL
      DO 320 ISUB = 1,3                                                 M32.8961
      DSUB2 = 1.D0 / ( ISUB**2 )                                        M39.3345
      DXEPK = .3D0 * DSUB2                                              M39.3346
      TOLERR = 1.01D0**DSUB2                                            M39.3347
      DFX = 10.D0**( .02D0*DSUB2/ISUB )                                 M39.3348
      DFPK1 = DFX**DJN1                                                 M39.3349
      DFPK2 = DFX**DJN2                                                 M39.3350
      DFPK11 = 1.D0 / DFPK1                                             M39.3351
      DFPK22 = 1.D0 / DFPK2                                             M39.3352
C   STORE ORIGINAL VALUES IN TEMPORARY VECTORS                          M32.8967
      DO 120 J = 1,JN1                                                  M32.8968
      FPZ1(J) = AZEPO(ID1+J)                                            M32.8969
      AZEPO(ID1+J) = AZEPO(ID1+J) * DFPK11                              M39.3353
  120 CONTINUE                                                          M32.8971
      DO 130 J = 1,JN2                                                  M32.8972
      FPZ2(J) = AZEPO(ID2+J)                                            M32.8973
      AZEPO(ID2+J) = AZEPO(ID2+J)*DFPK2                                 M32.8974
  130 CONTINUE                                                          M32.8975
C   SHIFT INWARDS                                                       M32.8976
C      ERROPT = 1.E12    !  THL 
      IDEFAC = -1                                                       M32.8978
      DO 190 JSH = 1,50                                                 M32.8979
      IDEFAC = IDEFAC+1                                                 M32.8980
      FCORN1 = AZEPO(ID1+JN1)*DFPK1                                     M32.8981
      FCORN2 = AZEPO(ID2+1) * DFPK22                                    M39.3354
      IF (FCORN1.GT.FCORN2) GO TO 200                                   M32.8983
      DO 140 J = 1,JN1                                                  M32.8984
  140 AZEPO(ID1+J) = AZEPO(ID1+J)*DFPK1                                 M32.8985
      DO 150 J = 1,JN2                                                  M32.8986
 150  AZEPO(ID2+J) = AZEPO(ID2+J) * DFPK22                              M39.3355
      CALL REFH                                                         M32.8988
C   CHECK ERROR                                                         M32.8989
      ERROR = 0.                                                        M32.8990
      X = XBEG-DXEPK                                                    M32.8991
  160 X = X+DXEPK                                                       M32.8992
      IF (X.GT.XEND) GO TO 170                                          M32.8993
      F = 10.D0**X                                                      M32.8994
      CALL RATP (F,AMAGLG)                                              M32.8995
      YAPPRX = AMAGLG                                                   M32.8996
      ERROR = ERROR+(YAPPRX-YFUN39(X))**2                               M33.2319
      GO TO 160                                                         M32.8998
  170 IF (IDEBUG.GE.4)                                                  M32.8999
     1 WRITE(LOUT,8073) IDEFAC,FCORN1,FCORN2,ERROR,ERROPT              
 8073 FORMAT(1X, 9HSHIFT IN ,I5,4E12.4)                                 
      IF (ERROR.GE.ERROPT) GO TO 180    ! THL   
      ERROPT = ERROR                                                    M32.9003
      IDEFOP = IDEFAC                                                   M32.9004
  180 IF (ERROR.GE.TOLERR*ERROPT) GO TO 200    ! THL   
  190 CONTINUE                                                          M32.9006
C   RESTORE ORIGINAL VALUES BEFORE SHIFTING IN THE OPPOSITE DIRECTION   M32.9007
  200 DO 210 J = 1,JN1                                                  M32.9008
  210 AZEPO(ID1+J) = FPZ1(J)                                            M32.9009
      DO 220 J = 1,JN2                                                  M32.9010
  220 AZEPO(ID2+J) = FPZ2(J)                                            M32.9011
C   SHIFT OUTWARDS                                                      M32.9012
      IDEFAC = 0                                                        M32.9013
      DO 280 JSH = 1,50                                                 M32.9014
      IDEFAC = IDEFAC-1                                                 M32.9015
      FCORN1 = AZEPO(ID1+1) * DFPK11                                    M39.3356
      FCORN2 = AZEPO(ID2+JN2)*DFPK2                                     M32.9017
      IF (FCORN1.LT.FLIM1) GO TO 290                                    M32.9018
      IF (FCORN2.GT.FLIM2) GO TO 290                                    M32.9019
      DO 230 J = 1,JN1                                                  M32.9020
 230  AZEPO(ID1+J) = AZEPO(ID1+J) * DFPK11                              M39.3357
      DO 240 J = 1,JN2                                                  M32.9022
  240 AZEPO(ID2+J) = AZEPO(ID2+J)*DFPK2                                 M32.9023
      CALL REFH                                                         M32.9024
C   CHECK ERROR                                                         M32.9025
      ERROR = 0.                                                        M32.9026
      X = XBEG-DXEPK                                                    M32.9027
  250 X = X+DXEPK                                                       M32.9028
      IF (X.GT.XEND) GO TO 260                                          M32.9029
      F = 10.D0**X                                                      M32.9030
      CALL RATP (F,AMAGLG)                                              M32.9031
      YAPPRX = AMAGLG                                                   M32.9032
      ERROR = ERROR+(YAPPRX-YFUN39(X))**2                               M33.2321
      GO TO 250                                                         M32.9034
  260 IF (IDEBUG.GE.4)                                                  M32.9035
     1 WRITE(LOUT,8075) IDEFAC,FCORN1,FCORN2,ERROR,ERROPT              
 8075 FORMAT(1X, 9HSHIFT OUT,I5,4E12.4)                                 
      IF (ERROR.GE.ERROPT) GO TO 270   ! THL            
      ERROPT = ERROR
      IDEFOP = IDEFAC                                                   M32.9040
  270 IF (ERROR.GE.TOLERR*ERROPT) GO TO 290     ! THL 
  280 CONTINUE                                                          M32.9042
C   SHIFT ACCORDING TO OPTIMUM VALUE                                    M32.9043
 290  DF11 = DFPK1**IDEFOP                                              M39.3358
      DO 300 J = 1,JN1                                                  M39.3359
 300  AZEPO(ID1+J) = FPZ1(J) * DF11                                     M39.3360
      DF22 = DFPK22**IDEFOP                                             M39.3361
      DO 310 J = 1,JN2                                                  M32.9046
 310  AZEPO(ID2+J) = FPZ2(J) * DF22                                     M39.3362
  320 CONTINUE                                                          M32.9048
  330 CONTINUE                                                          M32.9049
  340 RETURN                                                            M32.9050
C               TERMINATING CONDITIONS                                  M32.9051
 2100 WRITE(LOUT,8077) MDIMPZ                                          
 8077 FORMAT(//,1X,31H%%% DIMENSION OF FPZ1 OR FPZ2 (,I2,               
     1       36H) EXCEEDED. EXECUTION TERMINATED %%%   )                M33.2324
      CALL STOPTP                                                               
      RETURN                                                                    
      END                                                               M32.9056
      SUBROUTINE ADJCR                                                  M32.9057
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL39.FOR;/NOLIST'                                           
      INCLUDE  'DECK39.FOR;/NOLIST'                                           
      CALL REFH                                                         M32.9060
      XLAST = XDAT(NDATA)                                               M32.9061
      XFIRST = XDAT(1)                                                  M32.9062
      NZONE1 = NZONE-1                                                  M32.9063
      DO 460 ITER = 1,3                                                 M32.9064
      IF (IDEBUG.GE.3)                                                  M32.9065
     1 WRITE(LOUT,8079) ITER                                           
 8079 FORMAT(//,5X,26H*** SHIFTING ITERATION NO., I2,3X, 3H***)         
      IRANGT = 0                                                        M32.9068
C             SHIFTING OF ZONES                                   
      DO 340 IZONE = 1,NZONE
      NRANGE = NOPRAO(IZONE)                                            M32.9161
      NRANG1 = NRANGE-1                                                 M32.9162
      IF (IDEBUG.GE.3)                                                  M32.9163
     1 WRITE(LOUT,8081) IZONE,NRANGE                                 
 8081 FORMAT(/,1X,30H*** CORNERS ADJUSTMENT OF ZONE, I3, 1H;, 2X,       
     1        9HTHERE ARE, I3,2X,11H RANGES ***)      
      DO 330 IRANGE = 1,NRANGE                                          M32.9165
      IRANGT = IRANGT+1                                                 M32.9166
      CALL INRAN (IRANGT,INDX0,INDXR1,INDXR2,INDXR3,INDXL1,INDXL2)      M32.9167
      IF ( NZONE .EQ. 1 )  GO TO 180
      IF (NRANGE.EQ.1) GO TO 280                                        M32.9168
      IF (NRANGE.EQ.2) GO TO 290                                        M32.9169
      IF (NRANGE.EQ.3) GO TO 300                                        M32.9170
      IF (IRANGE.EQ.1) GO TO 240                                        M32.9171
      IF (IRANGE.EQ.2) GO TO 250                                        M32.9172
      IF (IRANGE.EQ.NRANG1) GO TO 260                                   M32.9173
      IF (IRANGE.EQ.NRANGE) GO TO 270                                   M32.9174
C   INTERMEDIATE RANGES                                                 M32.9175
  100 XBEG = AZEPO(INDXL2+1)                                            
      XBEGF = AZEPO(INDXL1+1)                                           M32.9177
      FBEG = 10.**XBEGF                                                 M32.9178
      XEND = AZEPO(INDXR3+1)                                            M32.9179
      XENDF = AZEPO(INDXR2+1)                                           M32.9180
      FEND = 10.**XENDF                                                 M32.9181
      GO TO 320                                                         M32.9182
C             SPECIAL CASE: ONE ZONE ONLY                               M32.9137
  180 IF (NRANGE.LE.2) GO TO 190                                        M32.9138
      IF (NRANGE.EQ.3) GO TO 200                                        M32.9139
      IF (IRANGE.EQ.1 ) GO TO 240
      IF ( IRANGE .EQ. 2 )  GO TO 250
      IF (IRANGE.GE.NRANG1) GO TO 210                                   M32.9141
      GO TO 100                                                         M32.9142
  190 XBEG = XFIRST                                                     M32.9143
      FBEG = 10.**(XBEG-1.)                                             M32.9144
      XEND = XLAST                                                      M32.9145
      FEND = 10.**(XEND+1.)                                             M32.9146
      GO TO 320
  200 IF (IRANGE.EQ.1) GO TO 240
      IF (IRANGE.EQ.2) GO TO 190                                        M32.9149
  210 XBEG = AZEPO(INDXL2+1)                                            M32.9150
      XBEGF = AZEPO(INDXL1+1)                                           M32.9151
      FBEG = 10.**XBEGF                                                 M32.9152
      XEND = XLAST                                                      M32.9153
      FEND = 10.**(XEND+1.)                                             M32.9154
      GO TO 320
C   FIRST RANGE                                                         M32.9183
  240 XBEG = AZEPO(INDXL1+1)                                            M32.9184
      XBEGF = AZEPO(INDX0+1)                                            M32.9185
      FBEG = 10.**XBEGF                                                 M32.9186
      IF ( IZONE .EQ. 1 )  FBEG = FBEG / 10.
      IF ( IZONE .EQ. NZONE  .AND. NRANGE .EQ. 2 )  GO TO 245
      XEND = AZEPO(INDXR3+1)                                            M32.9187
      FEND = AZEPO(INDXR1+3)                                            M32.9188
      IF ( IZONE .NE. 1 )  GO TO 320
      XENDF = AZEPO(INDXR2+1)
      FEND = 10. ** XENDF
      GO TO 320                                                         M32.9189
  245 XEND = XLAST
      FEND = AZEPO(INDXR1+3)
      GO TO 320
C   SECOND RANGE                                                        M32.9190
  250 XBEG = AZEPO(INDXL2+1)                                            M32.9191
      FBEG = AZEPO(INDX0)                                               M32.9192
      IF ( IZONE .EQ. 1 )  FBEG = FBEG /10.
      XEND = AZEPO(INDXR3+1)                                            M32.9193
      XENDF = AZEPO(INDXR2+1)                                           M32.9194
      FEND = 10.**XENDF                                                 M32.9195
      GO TO 320                                                         M32.9196
C   ONE-TO-LAST RANGE                                                   M32.9197
  260 XBEG = AZEPO(INDXL2+1)                                            M32.9198
      XBEGF = AZEPO(INDXL1+1)                                           M32.9199
      FBEG = 10.**XBEGF                                                 M32.9200
      IF ( IZONE .EQ. NZONE )  GO TO 285
      XEND = AZEPO(INDXR3+1)                                            M32.9201
      FEND = AZEPO(INDXR1+3)                                            M32.9202
      GO TO 320                                                         M32.9203
  285 XEND = XLAST
      FEND = 10. ** (XEND+1.)
      GO TO 320
C   LAST RANGE                                                          M32.9204
  270 XBEG = AZEPO(INDXL2+1)                                            M32.9205
      FBEG = AZEPO(INDX0)                                               M32.9206
      IF ( IZONE .EQ. NZONE )  GO TO 285
      XEND = AZEPO(INDXR2+1)                                            M32.9207
      XENDF = AZEPO(INDXR1+1)                                           M32.9208
      FEND = 10.**XENDF                                                 M32.9209
      GO TO 320                                                         M32.9210
C   SPECIAL CASES                                                       M32.9211
C   ONE-RANGE ZONE                                                      M32.9212
  280 XBEG = AZEPO(INDXL1+1)                                            M32.9213
      XBEGF = AZEPO(INDX0+1)                                            M32.9214
      FBEG = 10.**XBEGF                                                 M32.9215
      IF ( IZONE .EQ. 1 )  FBEG = FBEG / 10.
      IF ( IZONE .EQ. NZONE )  GO TO 285
      XEND = AZEPO(INDXR2+1)                                            M32.9216
      XENDF = AZEPO(INDXR1+1)                                           M32.9217
      FEND = 10.**XENDF                                                 M32.9218
      GO TO 320                                                         M32.9219
C   TWO-RANGE ZONE                                                      M32.9220
  290 IF (IRANGE.EQ.1) GO TO 240                                        M32.9221
      IF (IRANGE.EQ.2) GO TO 270                                        M32.9222
C   THREE-RANGE ZONE                                                    M32.9223
  300 IF (IRANGE.EQ.1) GO TO 240                                        M32.9224
      IF (IRANGE.EQ.2) GO TO 310                                        M32.9225
      IF (IRANGE.EQ.3) GO TO 270                                        M32.9226
C   SECOND RANGE (3-R CASE)                                             M32.9227
  310 XBEG = AZEPO(INDXL2+1)                                            M32.9228
      FBEG = AZEPO(INDX0)                                               M32.9229
      IF ( IZONE .EQ. 1 )  FBEG = FBEG / 10.
      XEND = AZEPO(INDXR3+1)                                            M32.9230
      FEND = AZEPO(INDXR1+3)                                            M32.9231
  320 CALL SHIRA (XBEG,XEND,FBEG,FEND,ITER,INDX0,IRANGE)                M32.9232
  330 CONTINUE                                                          M32.9233
  340 CONTINUE                                                          M32.9234
      IF ( IDEBUG .LT. 4 )  GO TO 460
      WRITE(LUNIT6,7083)              !  THL                                 
 7083 FORMAT(//, ' after do 340 loop in ADJCR', /,
     1  1X,4X, 5HXBEGR,3X,5HALPHA,10X,                          
     138HPOLES AND ZEROES AFTER ADJUSTMENT (HZ),/)                      M33.2254
      INDEX = 0                                                         M32.8409
      DO 620 IRANGE=1,NTOTRA                                            M32.8410
      ALPHA = AZEPO(INDEX+2)                                            M32.8411
      JN = ABSZ(ALPHA)+ONEHAV                                           M32.8412
      I1 = INDEX+1                                                      M32.8413
      I2 = I1+1+2*JN                                                    M32.8414
      WRITE(LUNIT6,7079) (AZEPO(I),I=I1,I2)  
 7079 FORMAT(1X,E12.4,F4.0,6E11.4,/,17X,6E11.4,/,17X,6E11.4) 
      INDEX = I2                                                        M32.8416
  620 CONTINUE  
  460 CONTINUE                                                          M32.9308
      RETURN                                                            M32.9309
      END                                                               M32.9310
      SUBROUTINE SHIRA (XBEG,XEND,FBEG,FEND,ITER,INDEX,IRANGE)          M32.9311
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION FPZ(50)                                                 VAX.3429
      INCLUDE  'LABL39.FOR;/NOLIST'                                           
      INCLUDE  'DECK39.FOR;/NOLIST'                                           
C  CHANGE THIS LIMIT IF DIMENSION IS CHANGED                            M32.9315
C   ORDER OF SLOPE IN SEGMENT * 2                                       M32.9316
      MDIFPZ = 100                                                      VAX.3430
      IFLGRH = 0                                                        M32.9318
      IF (IZONE.EQ.NZONE.AND.IRANGE.EQ.NRANGE.AND.IFTYPE.EQ.2)          M32.9319
     1 IFLGRH = 1                                                       M32.9320
      ALPHA = AZEPO(INDEX+2)                                            M32.9321
      JN = ABSZ(ALPHA)+ONEHAV                                           M32.9322
      J2N = 2*JN                                                        M32.9323
      IF (J2N.GT.MDIFPZ) GO TO 2100                                     M33.2328
      ID = INDEX+2                                                      M32.9325
      IF (IDEBUG.GE.3)                                                  M32.9326
     1 WRITE(LOUT,8083) IRANGE,XBEG,XEND                               
 8083 FORMAT(/,1X,5X,27HCORNERS ADJUSTMENT OF RANGE,I3, 2H :,           
     1       2X, 3HX1=,E10.4,3X, 3HX2=,E10.4)                           M33.2330
      D13 = 1.D0 / JN                                                   M39.3364
      D13 = SQRTZ( D13 )                                                M39.3365
      ERROPT = 1.D12    !  THL
      DO 250 ISUB = 1,3                                                 M32.9330
      DXECR = .5D0/ISUB                                                 M32.9331
      TOLERR = 1.01D0**(1./ISUB**2)                                     M32.9332
      DFX = 10.D0**( DXECR*.1D0 )                                       M39.3366
      FDELTA = DFX**D13                                                 M39.3367
      FDELT1 = 1.D0 / FDELTA                                                    
C   PRESERVE ORIGINAL POSITIONS AND PREPARE FOR SHIFTING                M32.9336
      DO 100 J = 1,J2N                                                  M32.9337
      FPZ(J) = AZEPO(ID+J)                                              M32.9338
      AZEPO(ID+J) = AZEPO(ID+J) * FDELT1                                M39.3369
  100 CONTINUE                                                          M32.9340
C   SHIFT TO THE RIGHT                                                  M32.9341
C      ERROPT = 1.D12       ! I THINK THIS SHOULD BE MOVED OUTSIDE LOOP
      IDEFAC = -1                                                       M32.9343
      IDEFOP = 0                                                        M32.9344
      DO 150 JSH = 1,50                                                 M32.9345
      FCORN2 = AZEPO(ID+J2N)*FDELTA                                     M32.9346
      IF (FCORN2.GT.FEND) GO TO 160                                     M32.9347
      IDEFAC = IDEFAC+1                                                 M32.9348
      DO 110 J = 1,J2N                                                  M32.9349
  110 AZEPO(ID+J) = AZEPO(ID+J)*FDELTA                                  M32.9350
C   CHECK ERROR                                                         M32.9351
      IF (IFLGRH.EQ.1) CALL REFH                                        M32.9352
      ERROR = 0.                                                        M32.9353
      X = XBEG-DXECR                                                    M32.9354
  120 X = X+DXECR                                                       M32.9355
      IF (X.GT.XEND) GO TO 130                                          M32.9356
      F = 10.D0**X                                                      M32.9357
      CALL RATP (F,AMAGLG)                                              M32.9358
      YAPPRX = AMAGLG                                                   M32.9359
      ERROR = ERROR+(YAPPRX-YFUN39(X))**2                               M33.2331
      GO TO 120                                                         M32.9361
  130 IF (IDEBUG.GE.4)                                                  M32.9362
     1 WRITE(LOUT,8085) IDEFAC,FCORN2,FEND,ERROR,ERROPT                
 8085 FORMAT(1X, 7HSHIFT R,I5,4E12.4)                                   
      IF (ERROR.GE.ERROPT) GO TO 140     ! THL  
      ERROPT = ERROR                                                    M32.9366
      IDEFOP = IDEFAC                                                   M32.9367
  140 IF (ERROR.GE.TOLERR*ERROPT) GO TO 160 ! THL   
  150 CONTINUE                                                          M32.9369
C   RESTORE ORIGINAL VALUES BEFORE SHIFTING TO THE LEFT                 M32.9370
  160 DO 170 J = 1,J2N                                                  M32.9371
  170 AZEPO(ID+J) = FPZ(J)                                              M32.9372
C   SHIFT TO THE LEFT                                                   M32.9373
      IDEFAC = 0                                                        M32.9374
      DO 220 JSH = 1,50                                                 M32.9375
      FCORN1 = AZEPO(ID+1) * FDELT1                                     M39.3370
      IF (FCORN1.LT.FBEG) GO TO 230                                     M32.9377
      IDEFAC = IDEFAC-1                                                 M32.9378
      DO 180 J = 1,J2N                                                  M32.9379
 180  AZEPO(ID+J) = AZEPO(ID+J) * FDELT1                                M39.3371
C   CHECK ERROR                                                         M32.9381
      IF (IFLGRH.EQ.1) CALL REFH                                        M32.9382
      ERROR = 0.                                                        M32.9383
      X = XBEG-DXECR                                                    M32.9384
  190 X = X+DXECR                                                       M32.9385
      IF (X.GT.XEND) GO TO 200                                          M32.9386
      F = 10.D0**X                                                      M32.9387
      CALL RATP (F,AMAGLG)                                              M32.9388
      YAPPRX = AMAGLG                                                   M32.9389
      ERROR = ERROR+(YAPPRX-YFUN39(X))**2                               M33.2333
      GO TO 190                                                         M32.9391
  200 IF (IDEBUG.GE.4)                                                  M32.9392
     1 WRITE(LOUT,8087) IDEFAC,FCORN1,FBEG,ERROR,ERROPT                
 8087 FORMAT(1X, 7HSHIFT L,I5,4E12.4)                                   
      IF (ERROR.GE.ERROPT) GO TO 210   !  THL 
      ERROPT = ERROR                                                    M32.9396
      IDEFOP = IDEFAC                                                   M32.9397
  210 IF (ERROR.GE.TOLERR*ERROPT) GO TO 230  !  THL  
  220 CONTINUE                                                          M32.9399
C   SHIFT ACCORDING TO OPTIMUM VALUE                                    M32.9400
 230  FDELT2 = FDELTA**IDEFOP                                           M39.3372
      DO 240 J = 1,J2N                                                  M39.3373
 240  AZEPO(ID+J) = FPZ(J) * FDELT2                                     M39.3374
  250 CONTINUE                                                          M32.9403
      IF (IFLGRH.EQ.1) CALL REFH                                        M32.9404
      RETURN                                                            M32.9405
C             TERMINATING CONDITIONS                                    M32.9406
 2100 WRITE(LOUT,8089) MDIFPZ                                          
 8089 FORMAT(//,1X,22H%%% DIMENSION OF FPZ (,I2,                        
     1       36H) EXCEEDED. EXECUTION TERMINATED %%%   )                M33.2336
      CALL STOPTP                                                               
      RETURN                                                                    
      END                                                               M32.9411
      SUBROUTINE INRAN (IRANGE,INDX0,INDXR1,INDXR2,INDXR3,INDXL1,INDXL2)M32.9412
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL39.FOR;/NOLIST'                                           
      INCLUDE  'DECK39.FOR;/NOLIST'                                           
      ID = IRANGE                                                       M32.9415
      INDX0 = INDXV(ID)                                                 M32.9416
      ID = ID+1                                                         M32.9417
      IF (ID.GT.NTOTRA) ID = NTOTRA                                     M32.9418
      INDXR1 = INDXV(ID)                                                M32.9419
      ID = ID+1                                                         M32.9420
      IF (ID.GT.NTOTRA) ID = NTOTRA                                     M32.9421
      INDXR2 = INDXV(ID)                                                M32.9422
      ID = ID+1                                                         M32.9423
      IF (ID.GT.NTOTRA) ID = NTOTRA                                     M32.9424
      INDXR3 = INDXV(ID)                                                M32.9425
      ID = IRANGE-1                                                     M32.9426
      IF (ID.LT.1) ID = 1                                               M32.9427
      INDXL1 = INDXV(ID)                                                M32.9428
      ID = ID-1                                                         M32.9429
      IF (ID.LT.1) ID = 1                                               M32.9430
      INDXL2 = INDXV(ID)                                                M32.9431
      RETURN                                                            M32.9432
      END                                                               M32.9433
      SUBROUTINE REFH                                                   M32.9434
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL39.FOR;/NOLIST'                                           
      INCLUDE  'DECK39.FOR;/NOLIST'                                           
      XREF = XDAT(1)                                                    M32.9437
      I2 = NTOTRA                                                       M32.9438
      IF (IFTYPE.EQ.2) I2 = NTOTRA-1                                    M32.9439
      HREF = 1.D0                                                       M32.9440
      IF (I2.EQ.0) GO TO 180                                            M32.9441
      DO 100 IRANGE = 1,I2                                              M32.9442
      ID = INDXV(IRANGE)                                                M32.9443
      ALPHA = AZEPO(ID+2)                                               M32.9444
      IF (ALPHA.GT.0.) GO TO 110                                        M32.9445
      JN = -ALPHA+ONEHAV                                                M32.9446
      INDEXP = ID+2                                                     M32.9447
      INDEXZ = INDEXP+JN                                                M32.9448
      GO TO 120                                                         M32.9449
  110 JN = ALPHA+ONEHAV                                                 M32.9450
      INDEXZ = ID+2                                                     M32.9451
      INDEXP = INDEXZ+JN                                                M32.9452
  120 DO 130 J=1,JN                                                     M32.9453
      FPOLE = AZEPO(INDEXP+J)                                           M32.9454
      FZERO = AZEPO(INDEXZ+J)                                           M32.9455
      HREF = HREF*FPOLE/FZERO                                           M32.9456
  130 CONTINUE                                                          M32.9457
  100 CONTINUE                                                          M32.9458
  180 HREFLG = ALOG1Z(HREF)                                             M32.9459
      IF (IFTYPE.EQ.1) GO TO 170                                        M32.9460
      ID = INDXV(NTOTRA)                                                M32.9461
      ALPHA = AZEPO(ID+2)                                               M32.9462
      JN = -ALPHA+ONEHAV                                                M32.9463
      INDEXP = ID+2                                                     M32.9464
      DO 140 J=1,JN                                                     M32.9465
      FPOLE = AZEPO(INDEXP+J)                                           M32.9466
      HREFLG = HREFLG+ALOG1Z(FPOLE)                                     M32.9467
  140 CONTINUE                                                          M32.9468
  170 HREFLG = HREFLG+YFUN39(XREF)                                      M33.2337
      RETURN                                                            M32.9470
      END                                                               M32.9471
      SUBROUTINE RATP (FREQ,AMAGLG)                                     M32.9472
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL39.FOR;/NOLIST'                                           
      INCLUDE  'DECK39.FOR;/NOLIST'                                           
      I2 = NTOTRA                                                       M32.9475
      IF (IFTYPE.EQ.2) I2 = NTOTRA-1                                    M32.9476
      FREQ2 = FREQ**2                                                   M32.9477
      AMAGP = 1.D0                                                      M32.9478
      IF (I2.EQ.0) GO TO 180                                            M32.9479
      DO 100 IRANGE = 1,I2                                              M32.9480
      ID = INDXV(IRANGE) + 2                                            M39.3376
      ALPHA = AZEPO( ID )                                               M39.3377
      IF (ALPHA.GT.0.) GO TO 110                                        M32.9483
      JN = -ALPHA+ONEHAV                                                M32.9484
      INDEXP = ID                                                       M39.3378
      INDEXZ = INDEXP+JN                                                M32.9486
      GO TO 120                                                         M32.9487
  110 JN = ALPHA+ONEHAV                                                 M32.9488
      INDEXZ = ID                                                       M39.3379
      INDEXP = INDEXZ+JN                                                M32.9490
  120 DO 130 J=1,JN                                                     M32.9491
      FPOLE = AZEPO(INDEXP+J)                                           M32.9492
      FZERO = AZEPO(INDEXZ+J)                                           M32.9493
      AMAGP = AMAGP*(FZERO**2+FREQ2)/(FPOLE**2+FREQ2)                   M32.9494
  130 CONTINUE                                                          M32.9495
  100 CONTINUE                                                          M32.9496
  180 AMAGLG = ALOG1Z(AMAGP)                                            M32.9497
      IF (IFTYPE.EQ.1) GO TO 170                                        M32.9498
      ID = INDXV(NTOTRA) + 2                                            M39.3380
      ALPHA = AZEPO( ID )                                               M39.3381
      JN = -ALPHA+ONEHAV                                                M32.9501
      DO 140 J=1,JN                                                     M32.9503
      FPOLE = AZEPO( ID+J )                                             M39.3382
      AMAGLG = AMAGLG-ALOG1Z(FPOLE**2+FREQ2)                            M32.9505
  140 CONTINUE                                                          M32.9506
  170 AMAGLG = HREFLG+AMAGLG/2.D0                                       M32.9507
      RETURN                                                            M32.9508
      END                                                               M32.9509
      SUBROUTINE SPLIT (XBEG,XEND,YCUTPU,XCUT)                          M32.9510
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL39.FOR;/NOLIST'                                           
      INCLUDE  'DECK39.FOR;/NOLIST'                                           
      DXSPLI = .1D0                                                     M32.9513
      XFIRST = XDAT(1)                                                  M32.9514
      XLAST = XDAT(NDATA)                                               M32.9515
      YCUT = REFA + YCUTPU * ( REFB - REFA )                            VAX.3431
      IF ( ABSZ(XBEG-XEND) .LT. 0.1 )  DXSPLI = .01D0  ! AVOID OVERSHOT
      IF (XBEG.GT.XEND) GO TO 130                                       M32.9517
      IF( REFA .LT. REFB )   GO TO 110                                  VAX.3432
 90   X = XBEG-DXSPLI                                                   M39.3387
  100 X1 = X                                                            M32.9520
      X = X+DXSPLI                                                      M32.9521
      IF (X.GT.XLAST) X = XLAST                                         M32.9522
      IF (YFUN39(X).LE.YCUT) GO TO 170                                  M33.2340
      IF( X .EQ. XLAST )  GO TO  110                                            
      GO TO 100                                                         M32.9524
  110 X = XBEG-DXSPLI                                                   M32.9525
  120 X1 = X                                                            M32.9526
      X = X+DXSPLI                                                      M32.9527
      IF (X.GT.XLAST) X = XLAST                                         M32.9528
      IF (YFUN39(X).GE.YCUT) GO TO 170                                  M33.2341
      IF( X .EQ. XLAST )  GO TO  90                                             
      GO TO 120                                                         M32.9530
 130  IF( REFA .GT. REFB )  GO TO 150                                   VAX.3433
 135  X = XBEG+DXSPLI                                                   M39.3391
  140 X1 = X                                                            M32.9533
      X = X-DXSPLI                                                      M32.9534
      IF (X.LT.XFIRST) X = XFIRST                                       M32.9535
      IF (YFUN39(X).GE.YCUT) GO TO 170                                  M33.2343
      IF( X .EQ. XFIRST )  GO TO 150                                            
      GO TO 140                                                         M32.9537
  150 X = XBEG+DXSPLI                                                   M32.9538
  160 X1 = X                                                            M32.9539
      X = X-DXSPLI                                                      M32.9540
      IF (X.LT.XFIRST) X = XFIRST                                       M32.9541
      IF (YFUN39(X).LE.YCUT) GO TO 170                                  M33.2344
      IF( X .EQ. XFIRST )  GO TO 135                                            
      GO TO 160                                                         M32.9543
  170 IF (X1.LT.XFIRST) X1 = XFIRST                                     M32.9544
      D1 = YFUN39(X1)                                                   M33.2345
      XCUT = X1+(X-X1)*(YCUT-D1)/(YFUN39(X)-D1)                         M33.2346
      RETURN                                                            M32.9547
      END                                                               M32.9548
      FUNCTION YFUN39(X)                                                M33.2347
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL39.FOR;/NOLIST'                                           
      INCLUDE  'DECK39.FOR;/NOLIST'                                           
      IF (X.LE.XDAT(1)) GO TO 120                                       M32.9552
      IF (X.GE.XDAT(NDATA)) GO TO 100                                   M32.9553
      AI = APTDEC*(X-XDAT(1))+1.D0                                      M32.9554
      I1 = AI                                                           M34. 177
      Y = (X-XDAT(I1))*(YDAT(I1+1)-YDAT(I1))/(XDAT(I1+1)-XDAT(I1))      M39.3394
      YFUN39 = Y + YDAT(I1)                                             M33.2349
      GO TO 110                                                         M32.9558
  120 YFUN39 = YDAT(1)                                                  M33.2350
      GO TO 110                                                         M32.9560
  100 YFUN39 = YDAT(NDATA)                                              M33.2351
  110 RETURN                                                            M32.9562
      END                                                               M32.9563
      FUNCTION APH(X)                                                   M32.9564
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'LABL39.FOR;/NOLIST'                                           
      INCLUDE  'DECK39.FOR;/NOLIST'                                           
      IF (X.LE.XDAT(1)) GO TO 120                                       M32.9567
      IF (X.GE.XDAT(NDATA)) GO TO 100                                   M32.9568
      AI = APTDEC*(X-XDAT(1))+1.D0                                      M32.9569
      I1 = AI                                                           M34. 178
      APH = ( X-XDAT(I1) ) * ( APHDAT(I1+1)-APHDAT(I1) )                M39.3395
     1 / ( XDAT(I1+1)-XDAT(I1) ) + APHDAT( I1 )                         M39.3396
      GO TO 110                                                         M32.9574
  120 APH = APHDAT(1)                                                   M32.9575
      GO TO 110                                                         M32.9576
  100 APH = APHDAT(NDATA)                                               M32.9577
  110 RETURN                                                            M32.9578
      END                                                               M32.9579
      SUBROUTINE FTPLOT (ICURVE,IMODE,NFITMX)
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      REAL*8       TEXT1,TEXT2,TEXT3,BLANK,PL                           M32.9581
      DIMENSION PL(92)                                                  M32.9582
      INCLUDE  'LABL39.FOR;/NOLIST'                                           
      INCLUDE  'DECK39.FOR;/NOLIST'                                           
      DATA TEXT1 / 1H0 /, TEXT2 / 1HI /, TEXT3 /1H* /, BLANK / 1H  /    M32.9585
      DO 200 J=1,92                                                     M32.9586
  200 PL(J) = BLANK                                                     M32.9587
      IF ( ICURVE .EQ. 2 .AND. NFITMX .EQ. 0 )  GO TO 120
      YMIN =  1.D20                                                     M32.9589
      YMAX =  -1.D20                                                    M32.9590
      DO 100 I=1,NDATA                                                  M32.9591
      D1 = YDAT(I)                                                      M32.9592
      IF (D1.GT.YMIN) GO TO 110                                         M32.9593
      YMIN = D1                                                         M32.9594
  110 IF (D1.LT.YMAX) GO TO 100                                         M32.9595
      YMAX = D1                                                         M32.9596
  100 CONTINUE                                                          M32.9597
      AMIN = .90D0*10.D0**YMIN                                          M32.9598
      AMAX = 1.1D0*10.D0**YMAX                                          M32.9599
      DFAMP = AMAX-AMIN                                                 M32.9600
      IF ( NFITMX .GT. 0 )  GO TO 112   !  Ti fitting
      WRITE(LOUT,8091) IMODE                                           
 8091 FORMAT ( //,                                                      
     1  38H PRINTER PLOT OF  YC  FITTING FOR MODE,  I3,/,               M43.3290
     3  35H PLOT OF YCMAG VS. YCEQMAG IN MHOS ,/,                       M43.3291
     5  26H PLOT SYMBOLS:   0 = YCMAG ,                                 M43.3292
     6  15H    I = YCEQMAG ,                                            M43.3293
     7  27H    * = INTERSECTION POINT.     )                            M32.9607
      WRITE(LOUT,8093) AMIN,AMAX                                       
 8093 FORMAT(/,3X,5HYCMAG,6X,7HYCEQMAG,15X,E10.4,74X,E10.4,/,           
     1 28X,4HFREQ,/,35X,2H.1,10(9H........1))                           M32.9610
      GO TO 130                                                         M32.9611
  112 WRITE(LOUT,114) ICURVE, IMODE                                     M44. 819
  114 FORMAT ( //,                                                      M43.3296
     1  41H PRINTER PLOT OF  Ti  FITTING FOR ELEMENT, 2I2,/,            M43.3297
     3  27H PLOT OF TiMAG VS. TiEQMAG  ,/,                              M43.3298
     5  26H PLOT SYMBOLS:   0 = TiMAG ,                                 M43.3299
     6  15H    I = TiEQMAG ,                                            M43.3300
     7  27H    * = INTERSECTION POINT.     )                            M43.3301
      WRITE(LOUT,116) AMIN,AMAX                                         M43.3302
  116 FORMAT(/,3X,5HTiMAG,6X,7HTiEQMAG,15X,E10.4,74X,E10.4,/,           M43.3303
     1 28X,4HFREQ,/,35X,2H.1,10(9H........1))                           M43.3304
      GO TO 130                                                         M43.3305
  120 IF (IFTYPE.EQ.1) GO TO 122                                        M43.3306
      AMIN = 0.                                                         M32.9613
      AMAX = 1.D0                                                       M32.9614
      DFAMP = 1.D0                                                      M32.9615
      GO TO 124                                                         M32.9616
  122 AMIN = 0.                                                         M32.9617
      AMAX = 1.5D0                                                      M32.9618
      DFAMP = 1.5D0                                                     M32.9619
  124 WRITE(LOUT,8095) IMODE                                           
 8095 FORMAT ( //,                                                      
     1  38H PRINTER PLOT OF  A1  FITTING FOR MODE,  I3,/,               M32.9622
     3  35H PLOT OF A1MAG VS. A1EQMAG IN P.U. ,/,                       M32.9623
     5  26H PLOT SYMBOLS:   0 = A1MAG ,                                 M32.9624
     6  15H    I = A1EQMAG ,                                            M32.9625
     7  27H    * = INTERSECTION POINT.     )                            M32.9626
      WRITE(LOUT,8097) AMIN,AMAX                                       
 8097 FORMAT(/, 3X,5HA1MAG,6X,7HA1EQMAG,15X,E10.4,74X,E10.4,/,          
     1 28X,4HFREQ,/,35X,2H.1,10(9H........1))                           M32.9629
  130 X = XDAT(1)-.2D0                                                  M32.9630
  810 X = X+.2D0                                                        M32.9631
      IF (X.GT.XDAT(NDATA)) GO TO 850                                           
      FREQ = 10.D0**X                                                           
      FREQ2 = FREQ**2                                                   M32.9634
      AMAGP = 1.D0                                                      M32.9635
      IF (NZEROR.EQ.0) GO TO 825                                        M32.9636
      DO 820 J=1,NZEROR                                                 M32.9637
      AMAGP = AMAGP*(FCZR(J)**2+FREQ2)/(FCPR(J)**2+FREQ2)               M32.9638
  820 CONTINUE                                                          M32.9639
  825 AMAGLG = ALOG1Z(AMAGP)                                            M32.9640
      IF (IFTYPE.EQ.1) GO TO 840                                        M32.9641
      J1 = NZEROR+1                                                     M32.9642
      DO 830 J=J1,NPOLER                                                M32.9643
      AMAGLG = AMAGLG-ALOG1Z(FCPR(J)**2+FREQ2)                          M32.9644
  830 CONTINUE                                                          M32.9645
  840 AMAGLG = HRFLGR+AMAGLG/2.D0                                       M32.9646
      AX = 10.D0**YFUN39(X)                                             M33.2352
      AMAGP = 10.D0**AMAGLG                                             M32.9648
      INTD1 = (AX-AMIN)*90.D0/DFAMP+1.5D0                               M32.9649
      INTD2 = (AMAGP-AMIN)*90.D0/DFAMP+1.5D0                            M32.9650
      IF (INTD1.LT.1.OR.INTD1.GT.91) INTD1 = 92                         M32.9651
      IF (INTD2.LT.1.OR.INTD2.GT.91) INTD2 = 92                         M32.9652
      PL(INTD1) = TEXT1                                                 M32.9653
      PL(INTD2) = TEXT2                                                 M32.9654
      IF (INTD1.EQ.INTD2)                                               M32.9655
     1  PL(INTD1) = TEXT3                                               M43.3307
      WRITE(LOUT,8099) AX,AMAGP,FREQ,( PL(I), I=1,91 )                 
 8099 FORMAT(1X,E10.4,2X,E10.4,2X,E10.4,1X,91A1)                        
      PL(INTD1) = BLANK                                                 M32.9658
      PL(INTD2) = BLANK                                                 M32.9659
      GO TO 810                                                         M32.9660
  850 RETURN                                                            M32.9661
      END                                                               M32.9662
      SUBROUTINE MISC39                                                 M32.9663
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABL39.FOR;/NOLIST'                                           
C     THIS MODULE IS CALLED BY "SUBR39"  OF                             M32.9666
C     OVERLAY 39 WHEN NEW MISCELLANEOUS DATA CARDS ARE REQUIRED         M32.9667
C     (POSSIBLY ONCE FOR EACH MODE,  IN THE MOST EXTREME CASE).         M32.9668
      REAL*8        TEXT1, TEXT2, TEXT3, TEXT4                          M32.9669
      DIMENSION  ITEMP(9)                                               M32.9670
      EQUIVALENCE ( KDEFLT, INDTV(1) )                                          
      DATA  TEXT1   /  6HDATA    /                                      M32.9671
      DATA  TEXT2   /  6HSELECT  /                                      M32.9672
      DATA  TEXT3   /  6HDEFAUL  /                                      M32.9673
      DATA  TEXT4   /  6HT       /                                      M32.9674
      IF ( IPRSUP  .GE.  6 )                                            M32.9675
     1 WRITE (LUNIT6, 2243)  MODIFY                                     M32.9676
 2243 FORMAT (  28H TOP OF "MISC39".   MODIFY =,  I4  )                 M32.9677
      IF ( MODIFY  .EQ.  0 )   GO TO 2268                               M32.9678
C     READ INPUT CARD USING CIMAGE                                      M32.9679
 2245 IF ( KDEFLT  .NE.  1 )                                            M32.9680
     1 CALL CIMAGE                                                      M32.9681
      DECODE (80, 2247, ABUFF(1))  BUS1, BUS3                           M32.9682
 2247 FORMAT (  2A6  )                                                  M32.9683
      IF ( BUS1  .NE.  TEXT1 )   GO TO 4063                             M32.9684
 2268 CONTINUE                                                          M32.9685
      DECODE (80, 2247, ABUFF(1))  BUS1, BUS3                           M32.9686
      IF ( BUS1  .NE.  TEXT3 )   GO TO 2274                             M32.9687
      IF ( BUS3  .NE.  TEXT4 )   GO TO 2274                             M32.9688
      WRITE (KUNIT6, 2270)                                              M35.8951
 2270 FORMAT (                                                          M32.9690
     1  47H+REQUEST FOR DEFAULT FITTING (= 3 BLANK CARDS).  )           M32.9691
      CALL INTERP                                                       M35.8952
      KDEFLT = 1                                                        M32.9692
C     ERASE "DEFAULT" FROM "ABUFF" (INSTALLATION-DEPENDENT):            M32.9693
      CALL DEFBLK ( ABUFF )                                             M32.9694
 2274 IF ( BUS1  .NE.  TEXT2 )   GO TO 2286                             M32.9695
      MODESK = 1                                                        M32.9696
      DECODE (80, 2281, ABUFF(1))  ITEMP                                M32.9697
 2281 FORMAT ( 8X,  9I8 )                                               M32.9698
      DO 2283  J=1, 9                                                   M32.9699
      IF ( ITEMP(J)  .EQ.  0 )   GO TO 2283                             M32.9700
      K = 1                                                             M32.9701
      IF ( ITEMP(J)  .LT.  0 )   K = 2                                  M32.9702
      N3 = IABSZ ( ITEMP(J) )                                           M32.9703
      MODSKP(K, N3) = 1                                                 M32.9704
 2283 CONTINUE                                                          M32.9705
C     READ INPUT CARD USING CIMAGE                                      M32.9706
      CALL CIMAGE                                                       M32.9707
      GO TO 2268                                                        M32.9708
 2286 CONTINUE                                                          M32.9709
      DECODE (80, 2289, ABUFF(1))  BUS2                                 M32.9710
 2289 FORMAT ( 66X,  A6 )                                               M32.9711
C     BEGIN CODE TO READ OVERALL PROGRAM MISCELLANEOUS PARAM.:          M32.9712
      DECODE (80, 4057, ABUFF(1))  IDEBUG,IPUNCH,KOUTPR,GMODE           M32.9713
 4057 FORMAT ( 8X,3I8,E8.0)                                             M32.9714
      IF ( GMODE.LE.0.0 .AND. METRIK.EQ.0 ) GMODE=.48D-7                M32.9715
      IF ( GMODE.LE.0.0 .AND. METRIK.EQ.1 ) GMODE=.30D-7                M32.9716
      IF ( KOUTPR  .EQ.  0 )   KOUTPR = 2                               M32.9717
      IF ( KOUTPR  .LT.  0 )   KOUTPR = 0                               M32.9718
      IF ( KDEFLT  .EQ.  0 )                                            M32.9719
     1 WRITE (KUNIT6, 4059) GMODE,IPUNCH,IDEBUG,KOUTPR                  M35.8953
 4059 FORMAT (  8H+PARAM. ,E8.1,3I8)                                    M32.9721
      IF ( MODIFY  .NE.  0 )   GO TO 2245                               M32.9722
      GO TO 9900                                                        M32.9723
 4063 IF ( MODIFY  .EQ.  2 )   GO TO 4387                               M32.9724
C     BEGIN CODE FOR MISCELLANEOUS PARAM. OF  ZC  FITTING               M32.9725
      DECODE (80, 4065, ABUFF)  NEXMIS,EPSTOL,NORMAX,IECODE,            M32.9726
     1        IFWTA,IFPLOT,IFDAT,INELIM                                 M32.9727
 4065 FORMAT (I8,E8.0,6I8)                                              M32.9728
      IF ( NEXMIS  .EQ.  0 )   NEXMIS = -1                              M32.9729
      IF (EPSTOL.EQ.0) EPSTOL = .3D0                                    M32.9730
      D1 = EPSTOL                                                       M32.9731
      D13 = 1.0 + EPSTOL / 100.                                         M32.9732
      EPSTOL = ALOG1Z ( D13 )**2                                        M32.9733
      IF ( NORMAX  .LE.  0 )   NORMAX  = 30                             M32.9734
      IF ( KDEFLT  .EQ.  0 )                                            M32.9735
     1 WRITE (KUNIT6, 4082)  NEXMIS,D1,NORMAX,IECODE,                   M35.8954
     2        IFWTA,IFPLOT,IFDAT,INELIM                                 M32.9737
 4082 FORMAT ( 8H+Yc FIT.,I3,E8.1,6I3)                                  M43.3308
      CALL INTERP                                                       M35.8955
      GO TO 9900                                                        M32.9739
C     BEGIN CODE FOR MISCELLANEOUS PARAM. OF A1 FITTING                 M32.9740
 4387 CONTINUE                                                          M32.9741
      DECODE (80, 4391, ABUFF(1))  NEXMIS,EPSTOL,NORMAX,IECODE,         M32.9742
     1        IFWTA,IFPLOT,IFDAT,INELIM,AMINA1                          M32.9743
 4391 FORMAT (I8,E8.0,6I8,E8.0)                                         M32.9744
      IF (EPSTOL.EQ.0) EPSTOL = .3D0                                    M32.9745
      D1 = EPSTOL                                                       M32.9746
      D13 = 1.0 + EPSTOL / 100.                                         M32.9747
      EPSTOL = ALOG1Z ( D13 )**2                                        M32.9748
      IF ( NORMAX .LE. 0 )   NORMAX  = 30                               M32.9749
      IF (AMINA1.EQ.0.) AMINA1 = .05D0                                  M32.9750
      IF ( KDEFLT  .EQ.  0 )                                            M32.9751
     1 WRITE (KUNIT6, 4396)  NEXMIS,D1,NORMAX,IECODE,                   M35.8956
     2        IFWTA,IFPLOT,IFDAT,INELIM,AMINA1                          M32.9753
 4396 FORMAT ( 8H+A1 FIT.,I3,E8.1,6I3,E8.1)                             M32.9754
      CALL INTERP                                                       M35.8957
 9900 IF ( IPRSUP  .GE.  6 )                                            M32.9755
     1 WRITE (LUNIT6, 9903)                                             M32.9756
 9903 FORMAT ( 15H EXIT "MISC39".  )                                    M32.9757
      RETURN                                                            M32.9758
      END                                                               M32.9759
      SUBROUTINE DEFBLK ( ABUFF )                                       M32.9760
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      REAL*8        ABUFF                                               M34. 179
      DIMENSION  ABUFF(1)                                               M32.9761
C     ALMOST-UNIVERSAL MODULE FOR BLANKING OUT "ABUFF" CARD             M32.9762
C     IMAGE OF "DEFAULT" DATA CARD OF "MARTI SETUP".   THE ONE          M32.9763
C     AND ONLY CALL IS BY MODULE "MISC39" OF OVERLAY 39.   IT           M32.9764
C     IS NOT UNIVERSAL ONLY FOR THOSE COMPUTERS (E.G., APOLLO)          M32.9765
C     WHICH HAVE  "CHARACTER ABUFF*80"  DECLARATION.                    M32.9766
      ABUFF(1) = ABUFF(5)                                               M32.9767
      ABUFF(2) = ABUFF(5)                                               M32.9768
      RETURN                                                            M32.9769
      END                                                               M32.9770
