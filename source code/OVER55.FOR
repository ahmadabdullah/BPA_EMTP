      SUBROUTINE OVER55
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      EQUIVALENCE                       (MONCAR(4),  ISW)               M39.3667
      IF ( IPRSUP  .GE.  1 )                                            M32,2297
     1 WRITE ( LUNIT6, 4567 )  KILL                                     M39.3668
 4567 FORMAT ( 24H BEGIN "OVER55".  KILL =,  I6  )                      M39.3669
      KILSAV = KILL                                                     M25.4743
      IF ( KILL  .LE.  0 )   GO TO 6633                                 M39.3670
 6624 CALL SUBR55                                                               
 6633 IF ( NCHAIN .EQ. 51 )       GO TO 99999                           M39.3672
      IF ( NCHAIN .EQ. 31 )       GO TO 99999                           M39.3673
      KILL = 0                                                          M39.3674
      IF ( KOL132  .EQ.  132 )   GO TO 6639                             M39.3675
      WRITE (LUNIT6, 6634)  ( LSTAT(I+20),  I=1, 26 )                   M39.3676
 6634 FORMAT ( /,  42H ACTUAL LIST SIZES FOR PRECEDING SOLUTION:        M39.3677
     1  ,/,  14H   SIZE  1-10:,  10I6                                   M39.3678
     2  ,/,  14H   SIZE 11-20:,  10I6                                   M39.3679
     3  ,/,  14H   SIZE 21-ON:,  10I6   )                               M39.3680
      GO TO 6645                                                        M39.3681
 6639 WRITE (LUNIT6, 6554)                                              M39.3682
 6554 FORMAT( /, 101H CORE STORAGE FIGURES FOR PRECEDING DATA CASE NOW CM39.3683
     1OMPLETED.  ---------------------------------------, 2X,           M39.3684
     2 7HPRESENT, 3X, 7HPROGRAM, /, 63H A VALUE OF  -9999 INDICATES DEFAM39.3685
     3ULT, WITH NO FIGURE AVAILABLE.  ,  41X, 6HFIGURE, 5X,             M39.3686
     4 12HLIMIT (NAME)    )                                             M39.3687
      WRITE(LUNIT6, 38021)  LSTAT(21), LBUS                             M39.3688
38021 FORMAT( 5X,  39HSIZE LIST 1.   NUMBER OF NETWORK NODES.  ,        M39.3689
     1  56X, 2I10, 7H (LBUS)   )                                        M39.3690
      WRITE(LUNIT6, 38022)  LSTAT(22), LBRNCH                           M39.3691
38022 FORMAT( 5X, 42HSIZE LIST 2.   NUMBER OF NETWORK BRANCHES.  ,      M39.3692
     1 53X, 2I10, 9H (LBRNCH)   )                                       M39.3693
      WRITE(LUNIT6, 38023)  LSTAT(23), LDATA                            M39.3694
38023 FORMAT( 5X, 55HSIZE LIST 3.   NUMBER OF DATA VALUES IN R, L, C TABM39.3695
     1LES.  , 40X, 2I10, 8H (LDATA)   )                                 M39.3696
      WRITE(LUNIT6, 38024)  LSTAT(24), LEXCT                            M39.3697
38024 FORMAT( 5X,  49HSIZE LIST 4.   NUMBER OF ENTRIES IN SOURCE TABLE.,M39.3698
     1 46X, 2I10, 8H (LEXCT)   )                                        M39.3699
      WRITE (LUNIT6, 38025) KTRLSW(3), IPRSOV(36), LSTAT(25), LYMAT     M39.3700
38025 FORMAT ( 5X,  34HSIZE LIST 5.   STORAGE FOR (Y) AND,              M39.3701
     1              20H TRIANGULARIZED (Y).,   6X,                      M39.3702
     2     11HNO. TIMES =,  I5,  3X,   9HFACTORS =,  I5,                M39.3703
     3     2X,  2I10,  8H (LYMAT)    )                                  M39.3704
      WRITE (LUNIT6, 38026)  KTRLSW(5),  LSTAT(26), LSWTCH              M39.3705
38026 FORMAT( 5X,  35HSIZE LIST 6.   NUMBER OF ENTRIES IN,              M39.3706
     1             14H SWITCH TABLE.,  15X,  11HNO. FLOPS =,            M39.3707
     2              I6,   14X, 2I10, 9H (LSWTCH)   )                    M39.3708
      WRITE (LUNIT6, 38027)  MAXBUS, LSIZE7                             M39.3709
38027 FORMAT ( 5X, 39HSIZE LIST 7.   NUMBER OF TOTAL DISTINCT,          M39.3710
     1             32H ALPHANUMERIC (A6) PROGRAM NAMES,                 M39.3711
     2           24X, 2I10,  9H (LSIZE7)   )                            M39.3712
      WRITE(LUNIT6, 38028)  LSTAT(28), LPAST                            M39.3713
38028 FORMAT( 5X, 67HSIZE LIST 8.   NUMBER OF PAST HISTORY POINTS FOR DIM39.3714
     1STRIBUTED LINES.   , 28X, 2I10, 8H (LPAST)    )                   M39.3715
      WRITE(LUNIT6, 38029)  LSTAT(29), LNONL                            M39.3716
38029 FORMAT( 5X, 44HSIZE LIST 9.   NUMBER OF NONLINEAR ELEMENTS.   ,   M39.3717
     1 51X, 2I10, 8H (LNONL)   )                                        M39.3718
      WRITE(LUNIT6, 38030)  LSTAT(30), LCHAR                            M39.3719
38030 FORMAT( 5X, 67HSIZE LIST 10.  NUMBER OF POINTS DEFINING NONLINEAR M39.3720
     1CHARACTERISTICS.   , 28X, 2I10, 8H (LCHAR)   )                    M39.3721
      WRITE (LUNIT6, 38031)  LSTAT(31), LSMOUT                          M39.3722
38031 FORMAT (5X,  66HSIZE LIST 11.  NUMBER OF BRANCH OR SELECTIVE-NODE-M39.3723
     1VOLTAGE OUTPUTS.   ,  29X,  2I10,  9H (LSMOUT)    )               M39.3724
      WRITE(LUNIT6, 38032)  LSTAT(32), LSIZ12                           M39.3725
38032 FORMAT( 5X, 92HSIZE LIST 12.  NUMBER OF OUTPUT QUANTITIES (LIMITEDM39.3726
     1 ONLY WHEN PRINTING MAX ABSOLUTE VALUES)., 3X, 2I10, 9H (LSIZ12) )M39.3727
      WRITE(LUNIT6, 38033)  LSTAT(33), LFDEP                            M39.3728
38033 FORMAT ( 5X,  68HSIZE LIST 13.  NUMBER OF 'WEIGHTING' FREQUENCY-DEM39.3729
     1PENDENT LINE MODES.,  27X,  2I10,  8H (LFDEP)   )                 M39.3730
      WRITE(LUNIT6, 38034)  LSTAT(34), LWT                              M39.3731
38034 FORMAT( 5X, 82HSIZE LIST 14.  NUMBER OF CELLS USED TO STORE FREQ.-M39.3732
     1DEPENDENCE WEIGHTING FUNCTIONS.   , 13X, 2I10, 6H (LWT)   )       M39.3733
      WRITE(LUNIT6, 38035)  LSTAT(35), LTAILS                           M39.3734
38035 FORMAT( 5X, 78HSIZE LIST 15.  NUMBER OF CELLS USED FOR EXPONENTIALM39.3735
     1-TAIL LINE-HISTORY STORAGE.   , 17X, 2I10, 9H (LTAILS)   )        M39.3736
      WRITE (LUNIT6, 38036)  LSTAT(36), LIMASS                          M39.3737
38036 FORMAT( 5X, 38HSIZE LIST 16.  TOTAL NUMBER OF TYPE-59,            M39.3738
     1            13H S.M. MASSES.,  44X, 2I10, 9H (LIMASS)   )         M39.3739
      WRITE (LUNIT6, 38037)  LSTAT(37), LSYN                            M39.3740
38037 FORMAT (5X,  54HSIZE LIST 17.  NUMBER OF DYNAMIC SYNCHRONOUS MACHIM39.3741
     1NES.   ,  41X,   2I10,  7H (LSYN)     )                           M39.3742
      WRITE (LUNIT6, 38038)  LSTAT(38), MAXPE                           M39.3743
38038 FORMAT (5X,  57HSIZE LIST 18.  NUMBER OF BRANCH POWER-AND-ENERGY OM39.3744
     1UTPUTS.     ,   38X,  2I10,  8H (MAXPE)    )                      M39.3745
      WRITE (LUNIT6, 38039)  LSTAT(39), LTACST                          M39.3746
38039 FORMAT (5X,  64HSIZE LIST 19.  FLOATING-POINT WORKING SPACE FOR ALM39.3747
     1L TACS ARRAYS. ,   31X,  2I10,  9H (LTACST)    )                  M39.3748
C     KTAB IS IN BLKCOM, SO IT CAN NOT BE EQUIVALENCED TO               M39.3749
      IF ( KTAB .LE. 0 )  GO TO 7272                                    M39.3750
      LSTAT(53) = LSTAT(63) - LSTAT(60) + LSTAT(53)                     M39.3751
      LSTAT(56) = LSTAT(56) - LSTAT(58) + 3                             M39.3752
      LSTAT(58) = KTAB                                                  M39.3753
      WRITE (LUNIT6, 3632)  ( K,  K=1, 8 ),                             M39.3754
     1 ( LSTAT(K), K=51, 58),   ( LSTAT(K), K=61, 68 )                  M39.3755
 3632 FORMAT ( 7X,  14HTACS TABLE NO.,   8I10     ,/,                   M39.3756
     1         7X,  14HPRESENT FIGURE,   8I10     ,/,                   M39.3757
     2         7X,  14HPROGRAM LIMIT ,   8I10         )                 M39.3758
 7272 WRITE (LUNIT6, 38040)  LSTAT(40), LFSEM                           M39.3759
38040 FORMAT (5X,                                                       M39.3760
     1  46HSIZE LIST 20.  RECURSIVE CONVOLUTION PARAMETER,              M39.3761
     2  42H STORAGE FOR NON-COPIED BRANCH COMPONENTS.,                  M39.3762
     3  7X,  2I10,  8H (LFSEM)   )                                      M39.3763
      WRITE (LUNIT6, 38041)  LSTAT(41), LFD                             M39.3764
38041 FORMAT (5X,  75HSIZE LIST 21.  TOTAL STORAGE CELLS FOR MODAL-PHASEM39.3765
     1 TRANSFORMATION MATRICES. ,  20X ,  2I10,  6H (LFD)    )          M39.3766
      WRITE (LUNIT6, 38042)  LSTAT(42), LHIST                           M39.3767
38042 FORMAT (5X,  34HSIZE LIST 22.  NUMBER OF CELLS FOR,               M39.3768
     1 21H CONVOLUTION HISTORY.,  40X,  2I10,  8H (LHIST)    )          M39.3769
      WRITE (LUNIT6, 38071)  LSTAT(43), LSIZ23                          M39.3770
38071 FORMAT( 5X,  83HSIZE LIST 23.  GIANT ARRAYS FOR RENUMBERING AND STM39.3771
     1EADY-STATE SOLUTION CALCULATIONS.,  12X,  2I10,  9H (LSIZ23)  )   M39.3772
      WRITE (LUNIT6, 38044)  NCOMP, LCOMP                               M39.3773
38044 FORMAT (5X,  72HSIZE LIST 24.  NUMBER OF PHASES OF COMPENSATION, BM39.3774
     1ASED ON MAXIMUM NODES.,  23X,  2I10,  8H (NCOMP)    )             M39.3775
      WRITE (LUNIT6, 38045)  LSTAT(45), LSPCUM                          M39.3776
38045 FORMAT (5X,  13HSIZE LIST 25.,                                    M39.3777
     1  49H  FLOATING-POINT WORKING SPACE FOR  U.M.  ARRAYS.,           M39.3778
     2  33X,  2I10,  9H (LSPCUM)     )                                  M39.3779
      WRITE (LUNIT6, 38046)  LSTAT(46), LSIZ26                          M39.3780
38046 FORMAT (5X,  39HSIZE LIST 26.  SQUARE OF MAXIMUM NUMBER,          M39.3781
     1             19H OF COUPLED PHASES., 37X, 2I10, 9H (LSIZ26)  )    M39.3782
 6645 IF ( KOL132  .EQ.  132 )                                          M39.3783
     1 WRITE (LUNIT6, 38003)                                            M39.3784
38003 FORMAT( 100H TIMING FIGURES (DECIMAL) CHARACTERIZING CASE SOLUTIONM39.3785
     1 SPEED.  -------------------------------------, 4X, 6HCP SEC,     M39.3786
     2 3X, 7HI/O SEC, 3X, 7HSUM SEC  )                                  M39.3787
      IF ( FLSTAT(7)  .NE.  -9999. )   GO TO 2601                       M39.3788
C     SPECIAL TIMING CODE FOR SUPPORTING PROGRAMS.                      M39.3789
      D6 = FLSTAT(9) + FLSTAT(1)                                        M39.3790
      D7 = FLSTAT(10) + FLSTAT(2)                                       M39.3791
      D1 = D6 + D7                                                      M39.3792
      IF ( KOL132  .EQ.  132 )                                          M39.3793
     1 WRITE (LUNIT6, 38010)   D6, D7, D1                               M39.3794
      IF ( KOL132  .EQ.  80 )                                           M39.3795
     1 WRITE (LUNIT6, 6651)  D6, D7, D1                                 M39.3796
 6651 FORMAT (  39H TOTAL CASE TIMING (CP, I/O, TOT), SEC:,             M39.3797
     1   1X,  3F10.3  )                                                 M39.3798
      LASTOV = NCHAIN                                                   M39.3799
      NCHAIN = 1                                                        M39.3800
      IF ( IPRSUP  .GE.  1 )                                            M39.3801
     1 WRITE (LUNIT6, 4568)  KILL                                       M39.3802
 4568 FORMAT ( 23H EXIT "OVER55".  KILL =,  I6  )                       M39.3803
      GO TO 99999                                                       M39.3804
 2601 D1 = FLSTAT(1) + FLSTAT(2)                                        M39.3805
      D4 = D1                                                           M39.3806
      IF ( KOL132  .EQ.  132 )                                          M39.3807
     1 WRITE(LUNIT6, 38004)  FLSTAT(1), FLSTAT(2), D1                   M39.3808
      IF ( KOL132  .EQ.  80 )                                           M39.3809
     1 WRITE (LUNIT6, 6652)  FLSTAT(1), FLSTAT(2), D1                   M39.3810
 6652 FORMAT (  29H SECONDS FOR OVERLAYS  1-6  :,                       M39.3811
     1   3F9.3,   22H  --- (CP;  I/O;  TOT)    )                        M39.3812
38004 FORMAT( 5X, 67HDATA INPUT, SORTING, AND RENUMBERING (PRE STEADY STM39.3813
     1ATE STUFF) .....   , 28X, 3F10.3 )                                M39.3814
      D1 = FLSTAT(3) + FLSTAT(4)                                        M39.3815
      D4 = D4 + D1                                                      M39.3816
      IF ( KOL132  .EQ.  132 )                                          M39.3817
     1 WRITE (LUNIT6, 38005)  FLSTAT(3), FLSTAT(4), D1                  M39.3818
38005 FORMAT( 5X, 47HSTEADY-STATE (S.S.) SOLUTION CALCULATIONS ..... ,  M39.3819
     1 48X, 3F10.3 )                                                    M39.3820
      IF ( KOL132  .EQ.  80 )                                           M39.3821
     1 WRITE (LUNIT6, 6653)  FLSTAT(3), FLSTAT(4), D1                   M39.3822
 6653 FORMAT (  29H SECONDS FOR OVERLAYS  7-12 :,  3F9.3  )             M39.3823
      D1 = FLSTAT(5) + FLSTAT(6)                                        M39.3824
      D4 = D4 + D1                                                      M39.3825
      IF ( KOL132  .EQ.  132 )                                          M39.3826
     1 WRITE(LUNIT6, 38006) FLSTAT(5), FLSTAT(6), D1                    M39.3827
38006 FORMAT( 5X, 53HPOST-S.S. TO PRE-INTEGRATION-SETUP CALCULATIONS ...M39.3828
     1..  , 42X, 3F10.3 )                                               M39.3829
      IF ( KOL132  .EQ.  80 )                                           M39.3830
     1 WRITE (LUNIT6, 6654)  FLSTAT(5), FLSTAT(6), D1                   M39.3831
 6654 FORMAT (  29H SECONDS FOR OVERLAYS 13-15 :,  3F9.3  )             M39.3832
      D1 = FLSTAT(7) + FLSTAT(8)                                        M39.3833
      D4 = D4 + D1                                                      M39.3834
      IF ( KOL132  .EQ.  132 )                                          M39.3835
     1 WRITE(LUNIT6, 38007)  FLSTAT(7), FLSTAT(8), D1                   M39.3836
38007 FORMAT( 5X, 54HINTEGRATION CALCULATION (TIME IN TIME-STEP LOOP) ..M39.3837
     1...  , 41X, 3F10.3   )                                            M39.3838
      IF ( KOL132  .EQ.  80 )                                           M39.3839
     1 WRITE (LUNIT6, 6655)  FLSTAT(7), FLSTAT(8), D1                   M39.3840
 6655 FORMAT (  29H SECONDS FOR TIME-STEP LOOP :,  3F9.3  )             M39.3841
      HMIN = FLSTAT(9)  +  FLSTAT(10)                                   M39.3842
      D4 = D4 + HMIN                                                    M39.3843
      IF ( KOL132  .EQ.  132 )                                          M39.3844
     1 WRITE (LUNIT6, 38008)  FLSTAT(9), FLSTAT(10), HMIN               M39.3845
38008 FORMAT( 5X,   65HCOMPUTER TIME IN PLOTTING OR STATISTICS TERMINATIM39.3846
     1ON OVERLAY ..... ,30X,3F10.3)                                     M39.3847
      IF ( KOL132  .EQ.  80 )                                           M39.3848
     1 WRITE (LUNIT6, 6656)   FLSTAT(9), FLSTAT(10), HMIN               M39.3849
 6656 FORMAT (  29H SECONDS AFTER DELTAT-LOOP  :,  3F9.3  )             M39.3850
      D1 = FLSTAT(11) + FLSTAT(12)                                      M39.3851
      D4 = D1 + D4                                                      M39.3852
      IF ( KOL132  .EQ.  132 )                                          M39.3853
     1 WRITE (LUNIT6, 38009)  FLSTAT(11), FLSTAT(12), D1                M39.3854
38009 FORMAT ( 5X,  37H'DELTAT'-CHANGE RESTART TIME  .......,  58X,     M39.3855
     1 3F10.3  ,/,  101X,  29H-----------------------------        )    M39.3856
      D2 = 0.0                                                          M39.3857
      D3 = 0.0                                                          M39.3858
      DO 38011 I=1, 12, 2                                               M39.3859
      D2 = D2 + FLSTAT(I)                                               M39.3860
38011 D3 = D3 + FLSTAT(I+1)                                             M39.3861
      IF ( KOL132  .EQ.  132 )                                          M39.3862
     1 WRITE (LUNIT6, 38010)  D2, D3, D4                                M39.3863
38010 FORMAT ( 93X, 7HTOTALS , 3F10.3, //,1X)                           M39.3864
      IF ( KOL132  .EQ.  80 )                                           M39.3865
     1 WRITE (LUNIT6, 6658)  D2, D3, D4                                 M39.3866
 6658 FORMAT (  29X,  27H-------------------------   ,/,                M39.3867
     1   20X,  9HTOTALS  :,  3F9.3  )                                   M39.3868
      IF ( ISW  .NE.  4444 )  GO TO 6673                                M39.3869
      ISW = -3344                                                               
      CALL SUBR55                                                       M39.3871
 6673 LASTOV = NCHAIN                                                   M39.3872
      NCHAIN = 1                                                        M39.3873
      KILL = 0                                                          M39.3874
      IF ( IPRSUP  .GE.  1 )                                            M39.3875
     1 WRITE ( LUNIT6, 4568 )  KILL                                     M39.3876
99999 RETURN                                                            M39.3877
      END                                                               M39.3878
      SUBROUTINE SUBR55                                                 M39.3879
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      EQUIVALENCE  (MONCAR(1),  KNT),   (MONCAR(4),  ISW)               M39.3881
      EQUIVALENCE                       (MONCAR(10), MTAPE)             M39.3882
      REAL*8         TEXT1, TEXT2, TEXT3, TEXT4, TEXT5                  M39.3883
      REAL*8         TEXT11, TEXT12, TEXT13, TEXT14, TEXT15             M39.3884
      REAL*8         TEXT16, TEXT17, TEXT18, TEXT19                     M39.3885
      DATA  TEXT1   / 6HBEGIN   /                                       M39.3886
      DATA  TEXT2   / 6HNEW     /                                       M39.3887
      DATA  TEXT3   / 6HDATA    /                                       M39.3888
      DATA  TEXT4   / 6HCASE    /                                       M39.3889
      DATA  TEXT5   / 6HBNDC    /                                       M39.3890
      DATA  TEXT11  / 6HEND     /                                       M39.3891
      DATA  TEXT12  / 6HLAST    /                                       M39.3892
      DATA  TEXT13  / 6HELDC    /                                       M39.3893
      DATA  TEXT14   /  6HSTATIS  /                                     M39.3894
      DATA  TEXT15   /  6HTICS    /                                     M39.3895
      DATA  TEXT16   /  6HOUTPUT  /                                     M39.3896
      DATA  TEXT17   /  6HSALVAG  /                                     M39.3897
      DATA  TEXT18   /  6HE       /                                     M39.3898
      DATA  TEXT19   /  6HSOS     /                                     M39.3899
      IF ( IPRSUP .GE. 1 )                                              M39.3900
     1 WRITE (LUNIT6, 1001)  KILL                                       M39.3901
 1001 FORMAT (  25H TOP OF "SUBR55".  KILL =,  I6  )                    M39.3902
      IF ( ISW  .EQ.  -3344 )  GO TO 6740                                       
      IF ( KILL .LE. 200 )  GO TO 4092                                          
      N1 = KILL - 200                                                   M25.4745
      GO TO (                                                           M25.4746
     1 6201, 6202, 6203, 6204, 6205, 6206, 6207, 6208, 6209, 6210,      M25.4747
     2 6211, 6212, 6213, 6214, 6215, 6216, 6217, 6218, 6219, 6220,      M25.4748
     3 6221, 6222, 6223, 6224, 6225, 6226, 6227, 6228, 6229, 6230,      M25.4749
     4 6231, 6232, 6233, 6234, 6235, 6236, 6237, 6238, 6239, 6240,      M28.9198
     5 6241 ),  N1                                                      M28.9199
      GO TO 6540                                                        M32,2300
 6201 WRITE (LUNIT6, 7201)  LSTAT(16)                                   M28.9200
 7201 FORMAT (5X, 106HTHE LAST-READ DATA CARD IS A REQUEST FOR THE FURTHM22.7359
     1ER (CONTINUED) SOLUTION OF A PREVIOUSLY-SOLVED EMTP DATA     ,/,  M22.7360
     2 5X, 107HCASE.   THE   'RESTART'   REQUEST SPECIFIES A PERMANENT FM22.7361
     3ILE IN WHICH IS STORED   /BLANK/   AND   /LABEL/ .           ,/,  M22.7362
     4 5X, 111HBUT EMTP DIMENSIONING OF THE PRESENT PROGRAM VERSION IS NM22.7363
     5OT IDENTICAL TO THAT FOR THE VERSION WHICH CREATED THE       ,/,  M22.7364
     6 5X,  97HPERMANENT FILE.   SPECIFICALLY, THE TOTAL LENGTH OF   /LAM22.7365
     7BEL/   FOR THE FILE-CREATING PROGRAM WAS,  I8,  10H   INTEGER  )  M22.7366
      WRITE (LUNIT6, 7301)  LTLABL                                      M22.7367
 7301 FORMAT (5X,  48HWORDS, WHILE THE CORRESPONDING PRESENT FIGURE IS, M22.7368
     1 I8,   45H .    ANY SUCH DISCREPANCY IS ILLEGAL.   AS A      ,/,  M22.7369
     2 5X, 118HGENERAL RULE, THE USER IS COUNSELED TO USE THE SAME PROGRM22.7370
     3AM VERSION FOR BOTH OPERATIONS, THEREBY GUARANTEEING SUCCESS.   ) M22.7371
      GO TO 6550                                                        M25.4751
 6202 CONTINUE                                                          M33.2407
      GO TO 6550                                                        M25.4752
 6203 WRITE (LUNIT6, 7203)                                              M23.6331
 7203 FORMAT (5X, 110HTHE DATA CASE NOW BEING SOLVED INVOLVES ONE OR MORM23.6332
     1E TYPE-96 HYSTERETIC INDUCTORS.   FINE.   HOWEVER, NO       ,/,   M23.6333
     2 5X, 110HSTEADY-STATE PHASOR SOLUTION FOR INITIAL CONDITIONS WAS RM23.6334
     3EQUESTED BY THE USER.   THIS COMBINATION IS PERMITTED       ,/,   M23.6335
     4 5X, 106HONLY IF THE SIMULATION BEGINS AS THE CONTINUATION OF A PRM23.6336
     5EVIOUSLY-HALTED RUN (WITH FIELD  'TSTART'  OF THE           ,/,   M23.6337
     6 5X, 116HFLOATING-POINT MISCELLANEOUS DATA CARD PUNCHED POSITIVE).M23.6338
     7   THE DATA CASE UNDER CONSIDERATION DOES NOT SATISFY THESE  ,/,  M23.6339
     8 5X,  43HRESTRICTIONS, SO SOLUTION SHALL BE STOPPED.        )     M23.6340
      GO TO 6550                                                        M25.4753
 6204 WRITE (LUNIT6, 7204)  FLSTAT(14), FLSTAT(15)                      M23.6342
 7204 FORMAT (5X, 104HTHE LAST-INPUTTED EMTP COMPONENT WAS A TYPE-96 HYSM23.6343
     1TERETIC INDUCTOR.   COLUMNS  27-32  AND  33-38  OF THE      ,/,   M23.6344
     2 5X,  97HBRANCH CARD ARE TO BE PUNCHED WITH  'I-STEADY'  AND   'PSM23.6345
     3I-STEADY' ,   RESPECTIVELY.   BUT VALUES,  E14.4,  6H   AND  ,/,  M23.6346
     4 5X,  E14.4, 106H   WERE READ FOR THESE TWO VARIABLES, WHICH REPREM23.6347
     5SENTS A POINT IN THE CURRENT-FLUX PLANE THAT LIES OUTSIDE   ,/,   M23.6348
     6 5X, 111HTHE USER-DEFINED MAJOR HYSTERESIS LOOP.   THE EMTP DOES NM23.6349
     7OT ALLOW SUCH SLOPPINESS (EVEN THOUGH THE RATIO MAY BE      ,/,   M23.6350
     8 5X,  58HCORRECT).   DEFINE A POINT WITHIN THE LOOP, AND TRY AGAINM23.6351
     9.   )                                                             M23.6352
      GO TO 6550                                                        M25.4754
 6205 WRITE (LUNIT6, 7205)  FLSTAT(16), FLSTAT(17)                      M23.6354
 7205 FORMAT (5X, 108HTHE LAST-INPUTTED EMTP COMPONENT WAS A TYPE-96 HYSM23.6355
     1TERETIC INDUCTOR.   COLUMNS  39-44  OF THE BRANCH CARD ARE  ,/,   M23.6356
     2 5X,  68HTO BE PUNCHED WITH A RESIDUAL (REMNANT) FLUX VALUE.   BUTM23.6357
     3 A VALUE OF,  E14.4,   27H   WAS READ FOR THIS, WHICH       ,/,   M23.6358
     4 5X, 115HEXCEEDS (IN ABSOLUTE VALUE) THE FLUX OF THE USER-INPUTTEDM23.6359
     5 MAJOR HYSTERESIS LOOP AT ZERO CURRENT.   THIS LATTER FLUX  ,/,   M23.6360
     6 5X,  8HVALUE IS,  E14.4,   91H .   THE RESULT IS A CURRENT-FLUX PM23.6361
     7OINT WHICH LIES OUTSIDE THE MAJOR HYSTERESIS LOOP, WHICH    ,/,   M23.6362
     8 5X,  87HIS IMPOSSIBLE.   PUNCH A LEGAL RESIDUAL FLUX VALUE IN COLM23.6363
     9UMNS  39-44 ,   AND TRY AGAIN.    )                               M23.6364
      GO TO 6550                                                        M25.4755
 6206 WRITE (LUNIT6, 7206)                                              M23.6366
 7206 FORMAT (5X, 114HTHE USER IS TRYING TO COMBINE  'STATISTICS'  OR  'M23.6367
     1SYSTEMATIC'  RESULTS USING THE   'TABULATE ENERGIZATION RESULTS'  M23.6368
     2 ,/, 5X, 107HFEATURE.   BUT NOT ALL OF THE PARTIAL RESULTS ARE COMM23.6369
     3PATIBLE.   FILES PREVIOUSLY ATTACHED AND READ HAVE THE      ,/,   M23.6370
     4 5X,  39HFOLLOWING CHARACTERISTIC PARAMETERS ...     )            M23.6371
      WRITE (LUNIT6, 7306)  ( LSTAT(I), I=11, 13), BUS2                 M23.6372
 7306 FORMAT (10X,  I5,                                                 M23.6373
     1  45H = NTOT    (NUMBER OF ELECTRIC NETWORK NODES)          ,/,   M23.6374
     2 10X, I5,  39H = NSTAT   (NUMBER OF OUTPUT VARIABLES)       ,/,   M23.6375
     3 10X, I5,  31H = KSWTCH  (NUMBER OF SWITCHES)               ,/,   M23.6376
     4  9X, A6,  42H = BUS(NTOT)  (NAME OF LAST NETWORK NODE).    )     M23.6377
      WRITE (LUNIT6, 7406)  LSTAT(17)                                   M23.6378
 7406 FORMAT (5X,  58HON THE OTHER HAND, THE MOST RECENTLY ATTACHED FILEM23.6379
     1, NUMBER,  I5,  38H   IN ORDER OF USER SPECIFICATION, HAS   ,/,   M23.6380
     2 5X,  43HTHE FOLLOWING DIFFERENT CHARACTERISTICS ...    )         M23.6381
      WRITE (LUNIT6, 7306)  NTOT, LSTAT(14), KSWTCH, BUS1               M27, 159
      WRITE (LUNIT6, 7506)                                              M23.6383
 7506 FORMAT (5X,  99HNEVER TRY TO COMBINE RESULTS WHICH BELONG TO DIFFEM23.6384
     1RENTLY-STRUCTURED PROBLEMS, AS IN THIS DATA CASE.     )           M23.6385
      GO TO 6550                                                        M25.4756
 6207 WRITE (LUNIT6, 7207)  TMAX, TENERG                                M23.6387
 7207 FORMAT (5X, 104HTHIS DATA CASE HAS  'STATISTICS'  SWITCHES, BUT ITM23.6388
     1 IS HIGHLY IMPROBABLE THAT ANY WOULD EVER CLOSE.   THE      ,/,   M23.6389
     2 5X,  50HTERMINATION TIME  'TMAX'  OF THE SIMULATION EQUALS,      M23.6390
     3  E14.4,  42H ,   WHILE ALL RANDOM SWITCH-CLOSING TIMES      ,/,  M23.6391
     4 5X,  6HEXCEED,  E14.4,   98H   SECONDS WITH  3*SIGMA  PROBABILITYM23.6392
     5.   SUCH A WASTE OF COMPUTER RESOURCES WILL NOT BE TOLERATED. ,/, M23.6393
     6 5X,  96HEITHER INCREASE  'TMAX'  BEYOND THIS LATTER FIGURE, OR APM23.6394
     7PROPRIATELY DECREASE THE CLOSING TIMES.     )                     M23.6395
      GO TO 6550                                                        M25.4757
 6208 CONTINUE                                                          M33.2408
      GO  TO  6550                                                      M25.4758
 6209 WRITE (LUNIT6, 7209)  EPSILN, LSTAT(17)                           M25.4759
 7209 FORMAT (5X,  32HTHE JACOBIAN MATRIX FOR A NEWTON,                 M25.4760
     1             33H SOLUTION OF ZINC-OXIDE ARRESTERS,                M25.4761
     2             31H HAS BEEN FOUND TO BE SINGULAR.   ,/,             M25.4762
     3        5X,  31HTHE TOLERANCE  'EPSILN'  EQUALS,                  M25.4763
     4     E12.3,  33H ,   WHILE THE ITERATION COUNT IS,                M25.4764
     5        I5,   2H .   )                                            M25.4765
      GO TO 7412                                                        M25.4766
 6210 WRITE(LUNIT6,7210)  LSTAT(14)                                     M23.6409
 7210 FORMAT(5X,101HTHE INITIALIZATION OF A SATURATED SYNCHRONOUS MACHINM23.6410
     1E HAS FAILED TO CONVERGE. THE MACHINE IN QUESTION,/, 5X,          M23.6411
     2 24HHAD THE FOLLOWING NUMBER, 2X, I6 )                            M23.6412
      GO TO 6550                                                        M25.4767
 6211 WRITE(LUNIT6,7211) LSTAT(14),FLSTAT(13),FLSTAT(14),FLSTAT(15)     M33.2409
 7211 FORMAT(5X,58HTHE PROGRAM WAS INPUTTING DATA FOR SYNCHRONOUS MACHINM23.6415
     1E NO., I8,37HA NON-POSITIVE SET OF SATURATION DATA, /, 5X, 62HFOR M23.6416
     2ONE OF THE AXIS HAS BEEN DETECTED. THE READ IN DATA FOLLOW,       M33.2410
     3 25H BELOW THIS LINE ........, /, 10X, 3E20.8, /, 5X, 104HIN A CASM33.2411
     4E OF AN UNSATURATED S.M. THIS KILL-CODE IS CAUSED BY A NONSPECIFIEM33.2412
     5D VALUE OF PARAMETER 'AGLINE'.)                                   M33.2413
      GO TO 6550                                                        M25.4768
 6212 WRITE (LUNIT6, 7212)  MAXZNO, EPSTOP, FLSTAT(14)                  M25.4769
 7212 FORMAT (5X,  35HA RIGOROUS SOLUTION FOR ONE OR MORE,              M25.4770
     1             33H ZINC-OXIDE ARRESTERS HAS FAILED.,                M25.4771
     2              8H   UP TO,  I5,                                    M25.4772
     3             33H   ITERATIONS (VARIABLE 'MAXZNO')  ,/,            M25.4773
     4        5X,  33HWERE ALLOWED TO DRIVE THE CURRENT,                M25.4774
     5             16H RESIDUALS BELOW,   E12.4,                        M25.4775
     6             32H   AMPERES (TOLERANCE 'EPSTOP').   ,/,            M25.4776
     7        5X,   3HBUT,   E13.4,                                     M25.4777
     8             31H   AMPERES REMAIN FOR A PROBLEM,                  M25.4778
     9             34H EQUATION, SO THE NEWTON ITERATION,               M25.4779
     9             14H HAS DIVERGED.     )                              M25.4780
 7412 WRITE (LUNIT6, 7512)  LSTAT(13), BUS1, BUS2                       M27, 160
 7512 FORMAT (5X,  35HBY WAY OF COMPONENT IDENTIFICATION,,              M25.4782
     1             10H THERE ARE,   I5,                                 M25.4783
     2             35H   COUPLED ELEMENTS WHICH ARE BEING,              M25.4784
     3             28H SOLVED SIMULTANEOUSLY, WITH      ,/,             M25.4785
     4        5X,  36HTHE FIRST OF THESE (IN ORDER OF DATA,             M25.4786
     5             26H INPUT) CONNECTING NODE  ',  A6,                  M25.4787
     6             13H'  TO NODE  ',  A6,                               M28.9201
     7             26H' .   THE FIRST ELEMENT IS       )                M25.4789
      WRITE (LUNIT6, 7612)  LSTAT(15), LSTAT(14), LSTAT(16), T          M27, 161
 7612 FORMAT (5X,  14HLOCATED IN ROW,  I5,                              M25.4791
     1             34H   OF THE NONLINEAR ELEMENT TABLE,,               M25.4792
     2             34H   WHILE THE LAST IS IN ROW NUMBER,               M25.4793
     3                     I5,   2H .    ,/,                            M25.4794
     4        5X,   9HA RANK OF,  I5,                                   M25.4795
     5             34H   EXISTS FOR  (ZTHEV) ,   AND THE,               M25.4796
     6             19H SIMULATION TIME IS,  E13.5,  5H SEC.  )          M25.4797
      WRITE (LUNIT6, 7712)                                              M28.9202
 7712 FORMAT (5X,  37HPOSSIBLE AMELIORATIVE ACTIONS INCLUDE,            M28.9203
     1             39H A DECREASE IN TIME-STEP SIZE "DELTAT",,          M28.9204
     2             32H OR AN INCREASE IN THE ITERATION                  M28.9205
     3  ,/,   5X,  34H LIMIT "MAXZNO", OR AN INCREASE IN,               M28.9206
     4             36H THE DIVERGENCE TOLERANCE "EPSTOP" . )            M28.9207
      GO TO 6550                                                        M25.4798
 6213 WRITE (LUNIT6, 7213)  LSTAT(15), LSTAT(16)                        M27, 162
 7213 FORMAT (5X,  33HWHILE READING  ZNO  ARRESTER DATA,                M27, 163
     1             32H CARDS, A STRUCTURAL (NUMBERING),                 M27, 164
     2             32H DEFECT WAS FOUND.   THIS IS FOR   ,/,            M27, 165
     3        5X,  24HNONLINEAR ELEMENT NUMBER,  I5,                    M27, 166
     4             32H   WHICH CORRESPONDS TO ARRESTER,                 M27, 167
     5              7H NUMBER,  I5,  2H .    )                          M27, 168
      WRITE (LUNIT6, 8213)  LSTAT(17), LSTAT(16)                        M28.9208
 8213 FORMAT (5X,  34H THE READ-IN IDENTIFICATION NUMBER,               M28.9209
     1  I8,   3X,  32HDOES NOT AGREE WITH THE ARRESTER,                 M28.9210
     2             16H NUMBER EQUAL TO,  I8,  2H .  )                   M28.9211
      GO TO 6550                                                        M27, 169
 6214 WRITE (LUNIT6, 7214)  BUS1                                        M28.9212
 7214 FORMAT (5X,  29HTHE EMTP IS IN THE PROCESS OF,                    M28.9213
     1             28H READING THE DATA ASSOCIATED,                     M28.9214
     2             23H WITH THE  TACS  DEVICE,                          M28.9215
     3     / 5X,   30H IDENTIFIED BY THE 6-CHARACTER,                   M28.9216
     4             16H (OUTPUT) NAME ', A6, 3H' . )                     M28.9217
      WRITE (LUNIT6, 7314)                                              M28.9218
 7314 FORMAT (5X,  32HTHIS IS A TYPE-58 DEVICE DEFINED,                 M28.9219
     1             36H BY THE FOLLOWING TRANSFER FUNCTION:,             M28.9220
     2   // 10X,   24HGAIN / ( D0 +  D1 * S  ),                         M28.9221
     3   //  5X,   35HTHE DENOMINATOR OF THIS FUNCTION IS,              M28.9222
     4             32H PRESENTLY FOUND TO HAVE A VALUE,                 M28.9223
     5             27H OF   0.0  ,  THUS CREATING,                      M28.9224
     6    /  5X,   33HA SINGULARITY IN THE SYSTEM.   IN,                M28.9225
     7             34H EFFECT, THIS DENOMINATOR IS INTER,               M28.9226
     8             17HNALLY TRANSFORMED )                               M28.9227
      WRITE (LUNIT6, 7414)  DELTAT                                      M28.9228
 7414 FORMAT (5X,  35HBY THE TRAPEZOIDAL RULE OF IMPLICIT,              M28.9229
     1             33H INTEGRATION INTO THE EXPRESSION:,                M28.9230
     2    // 10X,  29H( D0  +  D1 * 2.0 / DELTAT  ),                    M28.9231
     3    //  5X,  27HWITH THE VALUE OF DELTAT = ,                      M28.9232
     4             E14.6 ,                                              M28.9233
     5     /  5X,  34HCORRECT THIS SITUATION BY CHANGING,               M28.9234
     6             31H EITHER  D0,  D1,  OR  DELTAT . )                 M28.9235
      GO TO 6550                                                        M28.9236
 6215 WRITE (LUNIT6, 7214) BUS1                                         M28.9237
      WRITE (LUNIT6, 7215)                                              M28.9238
 7215 FORMAT (5X,  36HTHIS  TYPE-60  IF-DEVICE  RECOGNIZES,             M28.9239
     1             41H  3  AND ONLY  3  SEPARATE INPUT SIGNALS.         M28.9240
     2     // 5X,  40HOF THE  5  FIELDS AVAILABLE FOR DEFINING,         M28.9241
     3             11H THE INPUTS,                                      M28.9242
     4     / 10X,  35HEACH ONE OF THE FIRST THREE MUST BE,              M28.9243
     5             30H NON-BLANK  (COLUMNS 11 - 33 ),                   M28.9244
     6     / 10X,  33HAND THE TWO REMAINING FIELDS MUST,                M28.9245
     7             35H BE LEFT BLANK  ( COLUMNS 35 - 49 ) )             M28.9246
      GO TO 6550                                                        M28.9247
 6216 WRITE (LUNIT6, 7214) BUS1                                         M28.9248
      WRITE (LUNIT6, 7216)                                              M28.9249
 7216 FORMAT (5X,  33HTHIS  TYPE-61  DEVICE  SELECTS AS,                M28.9250
     1             33H OUTPUT  ONE OF THE POSSIBLY  8  ,                M28.9251
     2             33HCONNECTED INPUTS                 ,                M28.9252
     3   / 5X,     33HDEPENDING ON THE VALUE OF ANOTHER,                M28.9253
     4             40H TACS VARIABLE CALLED 'SELECTOR SIGNAL'.)         M28.9254
      WRITE (LUNIT6, 7316)                                              M28.9255
 7316 FORMAT (5X,  31HHOWEVER, THE USER HAS NEGLECTED,                  M28.9256
     1             33H TO IDENTIFY THE NAME OF THE TACS,                M28.9257
     2             40H VARIABLE THAT IS TO SERVE THIS PURPOSE.,         M28.9258
     3   / 5X,     37HTHE USER SHOULD SPECIFY THIS SELECTOR,            M28.9259
     4             33H SIGNAL IN THE  6-CHARACTER FIELD,                M28.9260
     5             23H OF COLUMNS  75 - 80  . )                         M38.5580
      GO TO 6550                                                        M28.9262
 6217 WRITE (LUNIT6, 7214)  BUS1                                        M28.9263
      WRITE (LUNIT6, 7217)  LSTAT( 17)                                  M28.9264
 7217 FORMAT (5X, 11HTHIS  TYPE-,    I2,                                M28.9265
     1            32H  MIN/MAX  DEVICE  WILL IDENTIFY,                  M28.9266
     2            38H EITHER MAXIMA OR MINIMA, DEPENDING ON,            M28.9267
     3     / 5X,  35HTHE NUMERICAL NALUE READ IN COLUMNS,               M28.9268
     4            28H  57 - 62  OF THE DATA CARD. )                     M28.9269
      WRITE (LUNIT6, 7317)  FLSTAT( 14)                                 M28.9270
 7317 FORMAT (5X,  34HTHIS VALUE MUST BE TYPED AS EITHER,               M28.9271
     1    / 10X,   35H   +1.0  TO INDICATE THAT A MAXIMUM,              M28.9272
     2             21H IS TO BE CALCULATED,,                            M28.9273
     3    / 10X,   35HOR -1.0   -     -      -    MINIMUM,              M28.9274
     4             21H    -     -     -   .,                            M28.9275
     5    // 5X,   30HTHE PRESENT VALUE WAS READ AS ,                   M28.9276
     6            F13.6 )                                               M28.9277
      GO TO 6550                                                        M28.9278
 6218 WRITE (LUNIT6,7218)  BUS1                                         M28.9279
 7218 FORMAT (5X,  33HTHE PROGRAM WAS READING THE USER-,                M28.9280
     1             38HDEFINED FREE-FORMAT FORTRAN EXPRESSION,           M28.9281
     2       / 5X, 35HFOR THE TACS VARIABLE IDENTIFIED BY,              M28.9282
     3             24H THE  6-CHARACTER NAME ',  A6, 3H' ,,             M28.9283
     4       / 5X, 36HWHEN THE FOLLOWING ILLEGAL SITUATION,             M28.9284
     5             14H WAS DETECTED: )                                  M28.9285
      I1 = LSTAT( 17)                                                   M28.9286
      IF ( I1 .GT. 6 )  GO TO 62180                                     M28.9287
      GO TO ( 62181, 62182, 62183, 62184, 62185, 62186), I1             M28.9288
62180 I1 = I1 - 6                                                       M28.9289
      GO TO ( 62187, 62188, 62189, 62190, 62191, 62192, 62193), I1      M28.9290
62181 WRITE (LUNIT6, 72181)                                             M28.9291
72181 FORMAT (10X, 42HA PARENTHESIS WAS OPENED AND NEVER CLOSED.)       M28.9292
      GO TO 6550                                                        M28.9293
62182 WRITE (LUNIT6, 72182)                                             M28.9294
72182 FORMAT (10X, 37HTHIS EXPRESSION CONTAINS NO ARGUMENT.)            M28.9295
      GO TO 6550                                                        M28.9296
62183 WRITE (LUNIT6, 72183)                                             M28.9297
72183 FORMAT (10X, 35HATTEMPT TO CLOSE A PARENTHESIS THAT,              M28.9298
     1             35H HAD NOT BEEN OPENED.              )              M28.9299
      GO TO 6550                                                        M28.9300
62184 WRITE (LUNIT6, 72184)  BUS2                                       M28.9301
72184 FORMAT (10X, 14HTHE OPERATOR ',  A6,                              M28.9302
     1             29H' IS THE LAST ELEMENT OF THIS,                    M28.9303
     2             29H FORTRAN EXPRESSION.         ,                    M28.9304
     3     / 10X,  35HISN'T THERE AN ARGUMENT MISSING ...)              M28.9305
      GO TO 6550                                                        M28.9306
62185 WRITE (LUNIT6, 72185)  BUS2, BUS3                                 M28.9307
72185 FORMAT (10X, 34HTHE TWO FOLLOWING ARGUMENTS CANNOT,               M28.9308
     1             34H BE ADJACENT:                     ,               M28.9309
     2    /10X,     1H',  A6, 4H'  ',  A6, 1H' )                        M28.9310
      GO TO 6550                                                        M28.9311
62186 WRITE (LUNIT6, 72186)  BUS2                                       M28.9312
72186 FORMAT (10X, 36HTHE FIRST ELEMENT OF THIS EXPRESSION,             M28.9313
     1             14H WAS READ AS ',  A6, 3H' .,                       M28.9314
     2    /10X,    36HCAN IT REALLY BE ...                )             M28.9315
      GO TO 6550                                                        M28.9316
62187 WRITE (LUNIT6, 72187)  BUS2                                       M28.9317
72187 FORMAT (10X, 30HMISSING  '('  AFTER FUNCTION ', A6,1H')           M28.9318
      GO TO 6550                                                        M28.9319
62188 WRITE (LUNIT6, 72188)                                             M28.9320
72188 FORMAT (10X, 36HPLEASE BREAK THIS MONSTRUOUSLY LARGE,             M28.9321
     1             36H EXPRESSION INTO SMALLER SECTIONS.  )             M28.9322
      GO TO 6550                                                        M28.9323
62189 WRITE (LUNIT6, 72189)  BUS2, BUS3                                 M28.9324
72189 FORMAT (10X, 35HTHIS EXPRESSION IS NOT HOMOGENEOUS.,              M28.9325
     1       /10X, 34HTHE TWO OPERATORS UPON WHICH THIS ,               M28.9326
     2             34HCONDITION WAS DETECTED ARE        ,               M28.9327
     3       /15X,  1H', A6, 7H' AND ', A6, 3H' . )                     M28.9328
      GO TO 6550                                                        M28.9329
62190 WRITE (LUNIT6, 72190) LSTAT( 16)                                  M28.9330
72190 FORMAT (10X, 32HTHE NUMERICAL ARGUMENT ENDING IN,                 M28.9331
     1              9H COLUMN  ,  I2,                                   M28.9332
     2     /10X,   34HIS MORE THAN  20  CHARACTERS LONG. )              M28.9333
      GO TO 6550                                                        M28.9334
62191 WRITE (LUNIT6, 72191)  LSTAT( 16)                                 M28.9335
72191 FORMAT (10X, 32HTHE ALPHANUMERIC ARGUMENT ENDING,                 M28.9336
     1             12H IN COLUMN  , I2,                                 M28.9337
     2    /10X,    32HIS MORE THAN  6 CHARACTERS LONG.)                 M28.9338
      GO TO 6550                                                        M28.9339
62192 WRITE (LUNIT6, 72192)  LSTAT( 16)                                 M28.9340
72192 FORMAT (10X, 31HUNRECOGNIZABLE LOGICAL OPERATOR,                  M28.9341
     1             14H NEAR COLUMN  , I2 )                              M28.9342
      GO TO 6550                                                        M28.9343
62193 WRITE( LUNIT6, 72193)  LSTAT( 16)                                 M28.9344
72193 FORMAT( 10X, 32HTHE NUMERICAL ARGUMENT ENDING IN,                 M28.9345
     1              8H COLUMN ,  I2,                                    M28.9346
     2     /10X,   37HCONTAINS MORE THAN ONE DECIMAL POINT. )           M28.9347
      GO TO 6550                                                        M28.9348
 6219 WRITE (LUNIT6, 7213)  LSTAT(16), LSTAT(15)                        M28.9349
      WRITE (LUNIT6, 7219)  FLSTAT(15)                                  M28.9350
 7219 FORMAT (5X,  32HA NON-POSITIVE REFERENCE VOLTAGE,                 M28.9351
     1              9H EQUAL TO,  E13.4,                                M28.9352
     2             29H   WAS SPECIFIED BY THE USER.   )                 M28.9353
      GO TO 6550                                                        M28.9354
 6220 WRITE (LUNIT6, 7220) LAST, LSTAT(15), IBR                         M30.1458
 7220 FORMAT ( 5X, 38H OVERFLOW OF STEADY-STATE TABLE SPACE.,           M30.1459
     1             30H   LIST 23 TABLES ARE SIZED AT,  I6,              M30.1460
     2             37H   WORDS,   WHICH IS INSUFFICIENT FOR  ,/,        M30.1461
     3         5X, 36HEVEN JUST THE FORMATION OF  (Y),  TO,             M30.1462
     4             40H SAY NOTHING OF LATER TRIANGULARIZATION.,         M30.1463
     5             35H   OVERFLOW HAS OCCURRED AFTER ONLY,  I5          M30.1464
     6   ,/,   5X, 38HBRANCHES HAVE BEEN PROCESSED, OUT OF A,           M30.1465
     7              9H TOTAL OF,  I5,   2H .   )                        M30.1466
      GO TO 6550                                                        M30.1467
 6221 WRITE (LUNIT6, 7221)                                              M30.1468
 7221 FORMAT ( 5X,  34H THE NUMBER OF PHASES IN THIS LINE,              M30.1469
     1              35H IS LARGER THAN THE TEMPORARY LIMIT,             M30.1470
     2              30H OF 10 FOR K. C. LEE MODELING.       )           M30.1471
      GO TO 6550                                                        M30.1472
 6222 WRITE (LUNIT6, 7222)  LSTAT(14), BUS1, BUS2, LSTAT(15)            M31.8297
 7222 FORMAT ( 5X,  28HTHE STEADY-STATE SOLUTION IS,                    M31.8298
     1              34H FOR TWO OR MORE FREQUENCIES WHICH,              M31.8299
     2              33H ARE NOT SEPARATED.   TROUBLE WAS,               M31.8300
     3              25H SPOTTED AT BRANCH NUMBER,  I5   ,/,             M31.8301
     4         5X,  21HWHICH CONNECTS BUS  ",  A6,                      M31.8302
     5              14H"  WITH BUS  ",  A6,  9H" .   ONE,               M31.8303
     6              29H SOURCE OF CONFLICT IS IN ROW,  I5 )             M31.8304
      GO TO 6550                                                        M31.8305
 6223 WRITE (LUNIT6, 7223)  LSTAT(14), BUS1, BUS2, LSTAT(15),           M31.8306
     1                      LSTAT(16)                                   M31.8307
 7223 FORMAT ( 5X,  28HTHE STEADY-STATE SOLUTION IS,                    M31.8308
     1              34H FOR TWO OR MORE FREQUENCIES WHICH,              M31.8309
     2              33H ARE NOT SEPARATED.   TROUBLE WAS,               M31.8310
     3              25H SPOTTED AT SWITCH NUMBER,  I5   ,/,             M31.8311
     4         5X,  21HWHICH CONNECTS BUS  ",  A6,                      M31.8312
     5              14H"  WITH BUS  ",  A6,  9H" .   THE,               M31.8313
     6              33H LEFT IS EXCITED BY SOURCE NUMBER,  I5           M31.8314
     7    ,/,  5X,  29HWHILE THE RIGHT IS EXCITED BY,                   M31.8315
     8              14H SOURCE NUMBER,  I6,  2H .  )                    M31.8316
      GO TO 6550                                                        M31.8317
 6224 WRITE (LUNIT6, 7224)                                              M32,2301
 7224 FORMAT ( 5X,  34HTHE LAST-READ SWITCH CARD HAS BOTH,              M32,2302
     1              33H NAMES IDENTICAL.   THE SWITCH IS,               M32,2303
     2              34H CLOSED ON ITSELF, AND HAS NO USE.   )           M32,2304
      GO TO 6550                                                        M32,2305
 6225 WRITE(LUNIT6, 7225)  LSTAT(14), LSTAT(15)                         M35.9740
 7225 FORMAT( 5X, 48HTHE USER HAS OVERFLOWED STORAGE WITHIN THE CABLE,  M35.9741
     1 57H CONNSTANTS SUPPORTING PROGRAM.   FOR THE CURRENT PROGRAM   ,/M35.9742
     2 5X,  48HVERSION, ONE IS LIMITED TO CASES HAVING NOT OVER, I4 ,   M35.9743
     3 15H CONDUCTORS OR , I4,   32H CABLES( THE STORAGE FOR A THREE  ,/M35.9744
     4 5X, 58HPHASE TRANSMISSION LINE IS EQUAL TO 5 CABLES IF IT HAS 2 ,M35.9745
     3,54H GROUND WIRES.  STORAGE FOR THE ARRAYS IN QUESTION HAS   ,/,  M37.9173
     4 5X, 115HBEEN OPTIMALLY (AND DYNAMICALLY) ALLOCATED SO AS TO USE AM35.9747
     5S MUCH OF THE EMTP CORE STORAGE AS IS AVAILABLE.   IT THUS   ,/,  M35.9748
     6 5X, 58HIS NOT THE FAULT OF THE CABLE-CONSTANTS SUPPORTING PROGRAMM35.9749
     7,58H ITSELF, BUT RATHER OF THE OVERALL EMTP DIMENSIONING, THAT  ,/M37.9174
     8 5X, 106HTHIS DATA CASE MUST BE REJECTED.   GO PROCURE OR MAKE ANOM35.9751
     9THER PROGRAM VERSION WHICH HAS MORE TOTAL TABULAR    )            M35.9752
      WRITE (LUNIT6, 7182)                                              M35.9753
 7182 FORMAT ( 5X, 49HSTORAGE SPACE, AND TRY THE JOB OVER AGAIN.   REME,M35.9754
     1 58HMBER, THE CABLE CONSTANTS CALCULATION REQUIRES THE STORAGE ,/,M35.9755
     2 5X, 103HOF FULL MATRICES, SO MEMORY REQUIREMENTS GO UP APPROXIMATM35.9756
     3ELY AS THE SQUARE OF THE NUMBER OF CONDUCTORS.   )                M35.9757
      GO TO 6550                                                        M35.9758
 6226 CONTINUE                                                          M29.6225
 6227 CONTINUE                                                          M29.6226
 6228 CONTINUE                                                          M29.6227
 6229 CONTINUE                                                          M29.6228
 6230 CONTINUE                                                          M29.6229
 6231 CONTINUE                                                          M29.6230
 6232 CONTINUE                                                          M29.6231
 6233 CONTINUE                                                          M29.6232
 6234 CONTINUE                                                          M29.6233
 6235 CONTINUE                                                          M29.6234
 6236 CONTINUE                                                          M29.6235
 6237 CONTINUE                                                          M29.6236
 6238 CONTINUE                                                          M29.6237
 6239 CONTINUE                                                          M29.6238
 6240 CONTINUE                                                          M29.6239
 6241 CONTINUE                                                          M29.6240
 6540 WRITE(LUNIT6,7540) KILL, LASTOV                                   M29.6241
 7540 FORMAT( / 19HINVALID  KILL  CODE,  I5,  5X,                       M29.6242
     1           8HLASTOV =,  I4  ,/,  1X  )                            M29.6243
 6550 IF ( IPNTV(1)  .NE.  -8888 )   GO TO 1429                         M25.4818
      KILL = IPNTV(3) + 1                                               M22.7401
      IF ( KILL  .LE.  IPNTV(2) )   GO TO 1417                          M22.7402
      KILL = 0                                                          M32,2306
      LASTOV = NCHAIN                                                   M22.7403
      NCHAIN = 1                                                        M22.7404
      IF ( IPRSUP  .GE.  1 )                                            M32,2307
     1 WRITE ( LUNIT6, 4568 )   KILL, IPNTV(2)                          M39.3905
 4568 FORMAT ( 30H EXIT MODULE  "SUBR55".  KILL,,                       M39.3906
     1         11H IPNTV(2) =,  2I6  )                                  M39.3907
      GO TO 9000                                                        M39.3908
 1417 WRITE (LUNIT6, 1424)  KILL                                        M22.7406
 1424 FORMAT ( /,  28H MESSAGE OF KILL-CODE NUMBER,  I4,  1H.  )        M22.7407
      LASTOV = NCHAIN                                                   M22.7408
      NCHAIN = 51                                                       M22.7409
      IF ( IPRSUP  .GE.  1 )                                            M32,2310
     1 WRITE ( LUNIT6, 4568)  KILL                                      M39.3909
      GO TO 9000                                                        M39.3910
 1429 IF ( JFLSOS  .EQ.  0 )   GO TO 1430                               M23.6419
      IF ( NENERG  .EQ.  0 )   GO TO 1430                               M23.6420
C     WRITE BOUNDING RECORDS IN THE CASE OF STATISTICS SALVAGE (SOS).   M23.6421
      D7 = -9999.                                                       M23.6422
      N15 = KSWTCH + 1                                                  M23.6423
      WRITE (LUNIT3)  ( D7, J=1, N15 )                                  M23.6424
      N15 = LSTAT(32)                                                   M23.6425
      WRITE (LUNIT9)  ( D7, J=1, N15 )                                  M23.6426
      CALL STATSV                                                       M23.6427
 1430 IF ( M4PLOT .NE. 1 )  GO TO 6645                                          
      CALL SPYING                                                               
 6645 IF( KILL .GT. 1 )  GO TO 4092                                     M39.3912
      WRITE (LUNIT6, 3391)  LSTAT                                       M19.3700
 3391 FORMAT ( /, 103H FOR   KILL = 1   ERROR STOPS, PROGRAM MAINTENANCEM19.3701
     1 MAY SOMETIMES WISH TO INSPECT THE CONTENTS OF ERROR-        ,/,  M19.3702
     2  64H INTERFACE VECTORS  'LSTAT'  AND  'FLSTAT' .   THESE FOLLOW .M19.3703
     3...   ,//,  16H VECTOR  'LSTAT'   ,/,  ( 1X, 10I13 ) )            M19.3704
      WRITE (LUNIT6, 3392)  FLSTAT                                      M19.3705
 3392 FORMAT ( /,   17H VECTOR  'FLSTAT'   ,/,  ( 1X, 10E13.4 ) )       M19.3706
      N1 = LSTAT(16)                                                       17698
      WRITE(LUNIT6, 4000 )  N1                                          M17.1898
 4000 FORMAT( /,  99H OF COURSE MAYBE THE USER WOULD LIKE SOME SUGGESTIO   17700
     1NS AS TO WHY THE TABLE IN QUESTION (LIST NUMBER , I2, 1H),   /,      17701
     2 115H HAS OVERFLOWED.   IF SO, READ ON, GOOD BUDDY.   THE EMTP HASM27, 177
     3 A LONG-ESTABLISHED POLICY OF MERITORIOUS AND LAUDABLE   ,/,         17703
     4   90H COOPERATION IN THE FORM OF CRYSTAL-CLEAR DIAGNOSTIC MESSAGE   17704
     5S, SUCH AS THE FOLLOWING .....    )                                  17705
      IF ( N1  .EQ.  99 )   GO TO 4499                                     17706
      GO TO (4001, 4002, 4003, 4004, 4005, 4006, 4007, 4008, 4009,         17707
     1 4010, 4011, 4012, 4013, 4014, 4015, 4016, 4017, 4018, 4019,      M15.1880
     2 4020, 4021, 4022, 4023, 4024, 4025, 4026, 4027, 4028, 4029),  N1 M16.7995
 4001 WRITE(LUNIT6, 4101)                                               M17.1899
 4101 FORMAT( 5X, 106HNETWORK NODES ARE OF COURSE DEFINED BY THE USER'S    17710
     1BRANCH AND SWITCH CARDS, BY THE NAMES WHICH IDENTIFY THE  ,/,        17711
     2 5X, 111HTWO ENDS OF THE ELEMENT (FIELDS  'BUS1'  AND  'BUS2'  OF    17712
     3THE DATA CARD, COLUMNS 3-14).   IN ADDITION, THERE ARE  ,/,          17713
     4 5X,  72HSEVERAL LESS-OBVIOUS WAYS IN WHICH NODES ARE ADDED TO THE   17714
     5 BUS TABLE ....   ,/,                                                17715
     6 8X, 107H1.  SWITCHED-R ELEMENTS (TYPE-92) AND SWITCHED-L ELEMENTS   17716
     7 (TYPE 93) EACH CREATE ONE INTERNAL NODE FOR EVERY   ,/,             17717
     8 12X,  13HSUCH ELEMENT.    )                                         17718
      WRITE(LUNIT6, 4201)                                               M17.1900
 4201 FORMAT( 8X,  65H2.  GROUND (BLANK NODE NAME) ALWAYS HAS AN ENTRY I   17720
     1N THE BUS LIST.   ,/,                                                17721
     2 8X, 112H3.  EACH SINGLE-PHASE SATURABLE TRANSFORMER COMPONENT ADD   17722
     3S ONE NODE (NODE NAME  'BUSTOP' ,  COLUMNS 39-44 OF THE  ,/,         17723
     4 12X,  62HCARD BEARING THE REQUEST WORD 'TRANSFORMER ' IN COLUMNS    17724
     53-14).     ,/,                                                       17725
     6 8X, 111H4.  EACH THREE-PHASE SATURABLE TRANSFORMER COMPONENT ADDS   17726
     7 ONE NODE (NODE NAME 'BUS3PH', READ FROM COLUMNS 27-32  ,/,          17727
     8 12X, 107HOF THE CARD BEARING THE REQUEST WORD 'TRANSFORMER THREE    17728
     9PHASE ' IN COLUMNS 3-26).   THIS IS A 4-TH WORD, IN   )              17729
      WRITE(LUNIT6, 4301)                                               M17.1901
 4301 FORMAT( 12X,  75HADDITION TO THE 3 WHICH ARE ADDED UNDER POINT 3,    17731
     1FOR A 3-PHASE TRANSFORMER.   )                                       17732
      GO TO 4099                                                           17733
 4002 WRITE(LUNIT6, 4102)                                               M17.1902
 4102 FORMAT( 5X, 107HNETWORK BRANCHES ARE OF COURSE DEFINED DIRECTLY BY   17735
     1 THE USER AS HE INPUTS BRANCH DATA.   YET THE COUNTING OF   ,/,      17736
     2 5X, 114HENTRIES IN THIS LINEAR BRANCH TABLE HAS SOME SUBTLE POINT   17737
     3S WHICH ARE WORTHY OF THE FOLLOWING DETAILED COMMENT ....   ,/,      17738
     4 8X, 109H1.  TRUE NONLINEAR ELEMENTS (TYPE CODES 92 OR 93) OR THE    17739
     5CONTINUOUS TIME-VARYING RESISTANCE ELEMENT (TYPE 91)    ,/,          17740
     6 12X, 112HNEVER CONTRIBUTE TO THE LINEAR BRANCH TABLE.   THESE ELE   17741
     7MENTS ARE PULLED OUTSIDE OF THE NETWORK, AND ARE HANDLED   ,/,       17742
     8 12X,  16HBY COMPENSATION.     )                                     17743
      WRITE(LUNIT6, 4202)                                               M17.1903
 4202 FORMAT( 8X, 108H2.  SWITCHED-RESISTANCE ELEMENTS (TYPE-92) EACH CO   17745
     1NTRIBUTE ONE ENTRY TO THE LINEAR BRANCH TABLE.   SWITCHED-    ,/,    17746
     2 12X,  60HINDUCTANCE ELEMENTS (TYPE 93) CONTRIBUTE TWO ENTRIES API   17747
     3ECE.    ,/,                                                          17748
     4 8X, 109H3.  EACH TYPE-99 PSEUDO-NONLINEAR RESISTANCE ELEMENT CONT   17749
     5RIBUTES AN ENTRY, UNLESS IT IS PARALLELED BY ANOTHER   ,/,           17750
     6 12X, 107HLINEAR BRANCH.   THESE ADDED VERY-HIGH-IMPEDANCE BRANCHE   17751
     7S SHOW UP WITH A CARD IMAGE AND DATA INTERPRETATION   ,/,            17752
     8 12X,  60HALMOST AS THOUGH THE USER HAD INPUTTED THE RESISTOR HIMS   17753
     9ELF.     )                                                           17754
      WRITE(LUNIT6, 4302)                                               M17.1904
 4302 FORMAT( 8X, 113H4.  EACH N-WINDING SINGLE-PHASE SATURABLE TRANSFOR   17756
     1MER COMPONENT ALWAYS INTERNALLY SETS UP   2(N-1) + 1   BRANCHES,/,   17757
     2 12X, 110HFOR EVERYTHING BUT THE MAGNETIZING BRANCH.   FOR THE LAT   17758
     3TER, A NONZERO MAGNETIZING RESISTANCE (FIELD  'RMAG' ,     ,/,       17759
     4 12X, 109HCOLUMNS 45-50 OF THE TRANSFORMER REQUEST CARD) WILL ADD    17760
     5AN ENTRY, AS WILL A SATURATION CHARACTERISTIC DEFINED   ,/,          17761
     6 12X, 107HBY EXACTLY ONE POINT.   A 3-PHASE SATURABLE-TRANSFORMER    17762
     7COMPONENT CONTRIBUTES ONLY IN THAT IT CONSISTS OF 3    ,/,           17763
     8 12X,  36HSINGLE-PHASE UNITS AS JUST DETAILED.    )                  17764
      GO TO 4099                                                           17765
 4003 WRITE(LUNIT6, 4103)                                               M17.1905
 4103 FORMAT (5X, 107HTHE R, L, C TABLES STORE FLOATING-POINT RESISTANCE   17767
     1, INDUCTANCE, AND CAPACITANCE PARAMETER VALUES ASSOCIATED   ,/,      17768
     2 5X, 113HWITH LUMPED-PARAMETER ELEMENTS.   ALTHOUGH SUCH VALUES AR   17769
     3E INPUTTED ON BRANCH CARDS (MOSTLY), THE USER SHOULD NOT   ,/,       17770
     4 5X, 109HCONFUSE THE PRESENT PARAMETER STORAGE WITH THE BRANCH-TAB   17771
     5LE STORAGE OF LIST 2.   CONTRIBUTIONS TO THE PRESENT        ,/,      17772
     6 5X, 113HLIST-3 TABLE BY DIFFERENT EMTP COMPONENTS ARE AS FOLLOWS,M27, 178
     7 ASSUMING NO USAGE OF THE REFERENCE-BRANCH OR REFERENCE-   ,/,       17774
     8 5X,  20HCOMPONENT IDEA .....        )                               17775
      WRITE (LUNIT6, 4203)                                              M17.1906
 4203 FORMAT (8X,  61H1.  EACH UNCOUPLED SERIES R-L-C BRANCH CONTRIBUTES   17777
     1 ONE ENTRY.     ,/,                                                  17778
     2 8X, 109H2.  EACH N-PHASE PI-CIRCUIT COMPONENT, OR EACH N-PHASE MU   17779
     3TUALLY-COUPLED R-L COMPONENT, CONTRIBUTES   N(N+1)/2       ,/,       17780
     4 12X,   8HENTRIES.     ,/,                                           17781
     5 8X, 108H3.  EACH SINGLE-PHASE N-WINDING SATURABLE-TRANSFORMER COM   17782
     6PONENT CONTRIBUTES   3N-2   ENTRIES, AT LEAST.   IF    ,/,           17783
     7 12X, 113HMAGNETIZING RESISTANCE  'RMAG'  IS USED, ADD ANOTHER ENT   17784
     8RY.   IF THE TRANSFORMER IS ACTUALLY LINEAR, WITH FINITE-   ,/,      17785
     9 12X,  54HSLOPE MAGNETIZATION CHARACTERISTIC, ADD ANOTHER ENTRY. )   17786
      WRITE (LUNIT6, 4303)                                              M17.1907
 4303 FORMAT (8X, 106H4.  A 3-PHASE SATURABLE TRANSFORMER HAS THE AFOREM   17788
     1ENTIONED ENTRIES IN THE TABLE FOR THE THREE SINGLE-PHASE    ,/,      17789
     2 12X,  97HTRANSFORMERS WHICH ARE SUB-COMPONENTS OF IT.   IN ADDITI   17790
     3ON, THERE ARE ALWAYS   3   EXTRA ENTRIES.    ,/,                     17791
     4 8X, 113H5.  IF A NETWORK USES ONE OR MORE TYPE-99 PSEUDO-NONLINEA   17792
     5R RESISTANCE ELEMENTS WHICH IS NOT PARALLELED BY ANOTHER   ,/,       17793
     6 12X,  99HLUMPED-PARAMETER BRANCH, ONE ENTRY IS ADDED TO THE TABLE   17794
     7 (FOR ALL SUCH ELEMENTS, NOT FOR EACH ONE).     ,/,                  17795
     8 8X,  81H6.  EACH SWITCHED-RESISTANCE ELEMENT (TYPE-92 SWITCH CARD   17796
     9) CONTRIBUTES ONE ENTRY.      )                                      17797
      WRITE (LUNIT6, 4403)                                              M17.1908
 4403 FORMAT (8X,  83H7.  EACH SWITCHED-INDUCTANCE ELEMENT (TYPE-93 SWIT   17799
     1CH CARD) CONTRIBUTES TWO ENTRIES.    ,/,                             17800
     2 8X, 104H8.  EACH  TYPE-16  SOURCE ELEMENT (SIMPLIFIED  AC/DC  CONM22.7426
     3VERTER REPRESENTATION) CONTRIBUTES TWO ENTRIES.            ,/,    M22.7427
     4 8X, 108H9.  EACH DISTRIBUTED-PARAMETER TRANSMISSION CIRCUIT CONTRM22.7428
     5IBUTES     N * (N+1) / 2    ENTRIES, WHERE  'N'  IS        )      M22.7429
      WRITE (LUNIT6, 4503)                                              M22.7430
 4503 FORMAT (12X, 109HTHE NUMBER OF PHASES OF THE LINE.   IN CASE OF SUM22.7431
     1CH OVERFLOW, THE  'PRESENT FIGURE'   WILL NOT INCLUDE THESE.  ,/, M22.7432
     2 5X, 110HWHERE REFERENCE-BRANCH OR REFERENCE-COMPONENT IDEAS ARE U   17801
     3SED, THERE GENERALLY IS NO CONTRIBUTION AT ALL TO THE   ,/,          17802
     4 5X, 110HR, L, C TABLES.   IN THIS CASE, THE PROGRAM SIMPLY MAKES    17803
     5REFERENCE TO PREVIOUSLY-STORED (AND HENCE PREVIOUSLY-    ,/,         17804
     6 5X,  21HCOUNTED) DATA VALUES.     )                                 17805
      GO TO 4099                                                           17806
 4004 WRITE(LUNIT6, 4104)                                               M17.1909
 4104 FORMAT (5X, 105HCOUNTING THE NUMBER OF ENTRIES IN THE SOURCE TABLE   17808
     1 (SIZE OF LIST 4) IS QUITE SIMPLE, AS PER THE FOLLOWING   ,/,        17809
     2 5X, 10HRULES ....   ,/,                                             17810
     3 8X, 106H1.  EACH CONVENTIONAL SOURCE COMPONENT (TYPE CODE 1 THROU   17811
     4GH 14, PUNCHED IN COLUMNS 1-2 OF THE SOURCE CARD)    ,/,             17812
     5 12X,  22HCONTRIBUTES ONE ENTRY.    ,/,                              17813
     6 8X, 106H2.  EACH TYPE-15 SOURCE COMPONENT (THE SIMPLIFIED AC/DC C   17814
     7ONVERTER MODEL, NEGLECTING RIPPLE ON THE DC SIDE)    ,/,             17815
     8 12X,  24HCONTRIBUTES TWO ENTRIES.     )                             17816
      WRITE (LUNIT6, 4204)                                              M17.1910
 4204 FORMAT (8X, 103H3.  EACH 3-PHASE DYNAMIC SYNCHRONOUS-MACHINE COMPO   17818
     1NENT (TYPE CODES 21, 22, 23 PUNCHED IN COLUMNS 1-2 OF    ,/,         17819
     2 12X,  52HCONSECUTIVE SOURCE CARDS) CONTRIBUTES THREE ENTRIES. ,/,   17820
     3 8X, 116H4.  EACH SWITCHED-RESISTANCE ELEMENT (TYPE CODE 92 PUNCHE   17821
     4D IN COLUMNS 1-2 OF THE SWITCH CARD) CONTRIBUTES 2 ENTRIES.    )     17822
      GO TO 4099                                                           17823
 4005 WRITE (LUNIT6, 4105)                                              M17.1911
 4105 FORMAT (5X, 107HLIST 5 OSTENSIBLY GIVES THE SIZE OF THE TABLE-OF-F   17825
     1ACTORS STORAGE (L-U DECOMPOSITION) FOR THE TRIANGULARIZED  ,/,       17826
     2 5X, 115HREAL EQUIVALENT NODAL ADMITTANCE MATRIX  (Y)  OF THE TIME   17827
     3-STEP LOOP.   AT EACH TIME-STEP, THE REAL MATRIX EQUATIONS   ,/,     17828
     4 5X, 116H (Y)V = I   ARE SOLVED FOR REAL NODE-VOLTAGE VECTOR  V ,    17829
     5  BY MEANS OF A REPEAT SOLUTION USING THE TABLE OF FACTORS.    ,/,   17830
     6 5X, 114HBECAUSE  (Y)  IS SYMMETRIC, ONLY THE UPPER-TRIANGULAR FAC   17831
     7TORS (INCLUDING THE DIAGONAL) ARE STORED.   THERE IS ONLY    ,/,     17832
     8 5X, 107HONE INTEGER WORD AND ONE FLOATING-POINT WORD FOR EACH FAC   17833
     9TOR, IT WILL BE NOTED (SEE BELOW).   NODE ORDERING     )             17834
      IF ( LSTAT(13)  .EQ.  1 )                                         M32,2338
     1 WRITE (LUNIT6, 4805)  LSTAT(14), KPARTB                          M38.5592
 4805 FORMAT (5X, 39HBEGINNING WITH "M32." VERSIONS,  LIST 7,           M32,2340
     1            30H STORAGE OF (YBB/YBC) IS BEING,                    M32,2341
     2            38H DESTROYED,  AND THE FULL (Y) IS ADDED             M32,2342
     3  ,/,  5X,  39HTO THE BOTTOM OF LIST 5 (FILLS FROM THE,           M32,2343
     4            39H BOTTOM UP).   BUT SPACE RAN OUT BEFORE,           M32,2344
     5            39H THE STORAGE OF (Y) IS FINISHED.   ONLY            M32,2345
     6  ,/,  I8,  36H   ROWS ARE DONE,  OUT OF A TOTAL OF,              M32,2346
     7       I4,  42H ,   AND FACTORING HAS NOT YET EVEN BEGUN. )       M32,2347
      IF ( LSTAT(13)  .EQ.  2 )                                         M32,2348
     1 WRITE (LUNIT6, 4905)  LSTAT(15), KPARTB                          M32,2349
 4905 FORMAT (5X, 39HBEGINNING WITH "M32." VERSIONS,  LIST 7,           M32,2350
     1            30H STORAGE OF (YBB/YBC) IS BEING,                    M32,2351
     2            38H DESTROYED,  AND THE FULL (Y) IS ADDED             M32,2352
     3  ,/,  5X,  39HTO THE BOTTOM OF LIST 5 (FILLS FROM THE,           M32,2353
     4            35H BOTTOM UP).   THE DOWNWARD-GROWING,               M32,2354
     5            37H FACTORS SPILLED OVER ONTO (Y) AT ROW              M32,2355
     6  ,/,  I8,  40H   OF THE TRIANGULARIZATION,  WHEREAS WE,          M32,2356
     7            24H MUST REACH ROW KPARTB =,  I4,                     M32,2357
     8            23H   TO END SUCCESSFULLY.     )                      M32,2358
      WRITE (LUNIT6, 4205)                                              M17.1912
 4205 FORMAT (5X, 43HTHE ORDER OF ELIMINATION (NODE RENUMBERING),       M32,2359
     1            37H IS CONSTRAINED ONLY IN THAT NODES OF,             M32,2360
     2            31H KNOWN VOLTAGE ARE FORCED LAST.   )                M32,2361
      GO TO 4099                                                           17874
 4006 WRITE (LUNIT6, 4106)                                              M17.1916
 4106 FORMAT ( 5X, 108HSWITCHES ARE COMPLETELY STRAIGHTFORWARD, BEING DE   17876
     1FINED ONLY BY SWITCH CARDS.   ONE ENTRY IN THE SWITCH TABLE   ,/,    17877
     2 5X, 112HIS CREATED FOR EVERY SWITCH CARD, WHETHER IT IS FOR AN OR   17878
     3DINARY SWITCH ( 'ITYPE'  OF COLUMNS 1-2 EQUAL TO ZERO),     ,/,      17879
     4 5X,  96HA SWITCHED RESISTANCE ELEMENT ( 'ITYPE' = 92), OR A SWITC   17880
     5HED-INDUCTANCE ELEMENT ( 'ITYPE' = 93).    )                         17881
      GO TO 4099                                                           17882
C       ???????????   LIST 7 IS PRESENTLY UNUSED   ????????????????     M32,2362
 4007 WRITE (LUNIT6, 4107)                                              M32,2363
 4107 FORMAT (    8H UNUSED    )                                        M32,2364
      GO TO 4099                                                           17903
 4008 WRITE (LUNIT6, 4108)                                              M17.1919
 4108 FORMAT (5X, 108HPAST-HISTORY POINTS FOR DISTRIBUTED-PARAMETER REPR   17905
     1ESENTATION OF TRANSMISSION LINES ARE STORED IN MODAL FORM,     ,/,   17906
     2 5X, 115HALWAYS.   EACH MODE REQUIRES STORAGE, WHERE THERE ARE AS    17907
     3MANY MODES AS THERE ARE COUPLED CONDUCTORS (E.G., A DOUBLE   ,/,     17908
     4 5X, 117HCIRCUIT LINE HAS 6 MODES.).   A CONSTANT-PARAMETER (FREQU   17909
     5ENCY-INDEPENDENT) MODE CONTRIBUTES    TAU/DELTAT    ENTRIES,   ,/,   17910
     6 5X, 111HWHERE  'TAU'  IS THE MODAL TRAVEL-TIME OF THE LINE,  'DEL   17911
     7TAT'  IS THE TIME-STEP SIZE, AND THE DIVISION INVOLVES    )          17912
      WRITE (LUNIT6, 4208)                                              M17.1920
 4208 FORMAT (5X, 105HINTEGER TRUNCATION FOLLOWED BY THE ADDITION OF UNI   17914
     1TY.   FOR A FREQUENCY-DEPENDENT MODE, MORE PAST-HISTORY   ,/,        17915
     2 5X, 114HTHAN THIS IS NEEDED, ENOUGH TO PERFORM THE  A2(T)  CONVOL   17916
     3UTION.   IN THE PRECEDING FORMULA, TAKE  'TAU'  TO BE THE    ,/,     17917
     4 5X,  93HTIME  'T2'  AT WHICH THE EXPONENTIAL TAIL ON  A2(T)  BEGI   17918
     5NS (TYPICALLY 3 TRAVEL-TIMES OR SO).     )                           17919
      GO TO 4099                                                           17920
 4009 WRITE (LUNIT6, 4109)                                              M17.1921
 4109 FORMAT ( 5X,  86HENTRIES IN THE NONLINEAR-ELEMENT TABLE ARE CREATE   17922
     1D BY THE FOLLOWING ELEMENT TYPES ....    ,/,                         17923
     2 8X,  79H1.  PIECEWISE-LINEAR TIME-VARYING RESISTANCE ELEMENTS  R(   17924
     3T) ,   BRANCH-TYPE 91.      ,/,                                      17925
     4 8X,  56H2.  TRUE NONLINEAR  V-I  CHARACTERISTIC, BRANCH-TYPE 92.    17926
     5  ,/,                                                                17927
     6 8X,  54H3.  TRUE NONLINEAR INDUCTANCE ELEMENT, BRANCH-TYPE 93.,/,   17928
     7 8X,  71H4.  STAIRCASE TIME-VARYING RESISTANCE ELEMENT  R(T) ,   B   17929
     8RANCH-TYPE 97.         )                                             17930
      WRITE (LUNIT6, 4209)                                              M17.1922
 4209 FORMAT (8X,  56H5.  PSEUDO-NONLINEAR INDUCTANCE ELEMENT, BRANCH-TY   17932
     1PE 98.     ,/,                                                       17933
     2 8X,  58H6.  PSEUDO-NONLINEAR  V-I  CHARACTERISTIC, BRANCH-TYPE 99   17934
     3.   ,/,                                                              17935
     4 5X,  79HEVERY ELEMENT FALLING INTO THIS CLASSIFICATION CONTRIBUTE   17936
     5S ONE ENTRY TO LIST 9.     )                                         17937
      GO TO 4099                                                           17938
 4010 WRITE (LUNIT6, 4110)                                              M17.1923
 4110 FORMAT (5X, 108HTHIS LIST-10 STORAGE APPLIES TO ALL CHARACTERISTIC   17940
     1S WHICH ARE DEFINED AS PAIRS OF COORDINATES, TERMINATED BY    ,/,    17941
     2 5X,    108HA  9999-CARD.   EACH PAIR OF COORDINATES SO SEEN ON THM14.4668
     3E INPUT-DATA LISTING CONTRIBUTES ONE ENTRY TO LIST 10.   ,/,         17943
     4 5X, 111HBUT NOTE CAREFULLY THE WORDING OF THIS RULE.   IT IS ONLY   17944
     5 THE ONES WHICH ARE ACTUALLY SEEN VISUALLY ON THE DATA   ,/,         17945
     6 5X, 113HLISTING (USE OF THE REFERENCE-BRANCH PROCEDURE ADDS NOTHI   17946
     7NG TO LIST 10, AND WILL NOT BE SEEN ON THE DATA LISTING.   )         17947
      WRITE (LUNIT6, 4210)                                              M17.1924
 4210 FORMAT (/, 5X, 105HA SECOND CONTRIBUTOR TO THE LIST-10 STORAGE REQM14.4670
     1UIREMENT IS THE TYPE-94 NONLINEAR ELEMENT COMPONENT (SURGE     ,/,M14.4671
     2 5X, 110HARRESTER WITH CURRENT LIMITING GAP).   EACH SUCH SURGE ARM14.4672
     3RESTER WHICH DOES NOT USE THE REFERENCE-BRANCH OPTION        ,/,  M14.4673
     4 5X, 115HADDS  18  ENTRIES TO THE LIST-10 STORAGE REQUIREMENT.   FM14.4674
     5OR EACH SURGE ARRESTER WHICH DOES USE THE REFERENCE-BRANCH     ,/,M14.4675
     6 5X,  86HPROCEDURE, THERE IS A CONTRIBUTION OF  11  ENTRIES TO THEM14.4676
     7 LIST-10 STORAGE REQUIREMENT.              )                      M14.4677
      WRITE (LUNIT6, 4310)                                              M32,2365
 4310 FORMAT (/, 5X, 118HFINALLY, IF YOU HAVE ZNO SURGE ARRESTERS IN THEM32,2366
     1 CASE, FOUR ADDTIONAL CELLS ARE REQUIRED FOR EACH OF THE ZNO ARRESM32,2367
     2TERS.  )                                                          M32,2368
      GO TO 4099                                                           17948
 4011 WRITE (LUNIT6, 4111)                                              M17.1925
 4111 FORMAT (5X, 109HBRANCH-OUTPUT QUANTITIES ARE GENERATED BY COLUMN-8   17950
     10 PUNCHES ON BRANCH CARDS AND ON SWITCH CARDS.   EACH PUNCH    ,/,   17951
     2 5X, 110HOF  '1'  OR  '2'  (BRANCH CURRENT OR BRANCH VOLTAGE) CONT   17952
     3RIBUTES ONE ENTRY TO LIST 11.   PUNCHES OF  '3'  (FOR   ,/,          17953
     4 5X, 107HBRANCH CURRENT AND VOLTAGE) OR  '4'  (FOR BRANCH POWER AN   17954
     5D ENERGY) CONTRIBUTE TWO ENTRIES EACH, TO LIST 11.     ,/,           17955
     6 5X, 110HNODE-VOLTAGE OUTPUTS WHICH ARE SPECIFIED INDIVIDUALLY, ON   17956
     7E AT A TIME, ----- I.E., BY PUNCHING 6-CHARACTER NODE      )         17957
      WRITE (LUNIT6, 4211)                                              M17.1926
 4211 FORMAT (5X, 110HNAMES IN THE  13A6  FIELD OF THE NODE-VOLTAGE OUTP   17959
     1UT-SPECIFICATION CARD ----- ARE LIKEWISE LIMITED BY LIST 11.   ,/,   17960
     2 5X, 111HIF THE USER HAS REQUESTED THE AUTOMATIC OUTPUT OF EVERY N   17961
     3ODE VOLTAGE INSTEAD OF THIS SELECTIVE OUTPUT (BY MEANS      ,/,      17962
     4 5X, 109HOF A  '1'  PUNCHED IN COLUMN 2 OF THE AFOREMENTIONED CARD   17963
     5), THIS LIST-11 LIMIT DOES NOT APPLY TO NODE VOLTAGE  ,/,            17964
     6 5X,   8HOUTPUTS.    )                                               17965
      GO TO 4099                                                           17966
 4012 WRITE (LUNIT6, 4112)                                              M17.1927
 4112 FORMAT ( 5X,  35HSORRY, NO SPECIAL ADVICE AVAILABLE.     )           17968
      GO TO 4099                                                           17969
 4013 WRITE (LUNIT6, 4113)                                              M17.1928
 4113 FORMAT (5X, 110HEVERY CONTINUOUSLY-TRANSPOSED DISTRIBUTED-PARAMETE   17971
     1R TRANSMISSION-LINE COMPONENT (BRANCH TYPE-CODE  'ITYPE'  OF   ,/,   17972
     2 5X, 110HCOLUMNS 1-2 EQUAL TO  -1,  -2,  ETC.) REPRESENTS A POSSIB   17973
     3LE CONTRIBUTION TO LIST 13.   EACH LINE MODE WHICH IS   ,/,          17974
     4 5X, 113HMODELLED AS BEING FREQUENCY-DEPENDENT (VARIABLE  'IPUNCH'   17975
     5  OF COLUMNS 53-54 EQUAL TO  -1 )  CONTRIBUTES ONE ENTRY   ,/,       17976
     6 5X, 110HTO LIST 13.   GENERALLY THIS WILL ONLY BE FOR THE ZERO-SE   17977
     7QUENCE MODE (THE FIRST CARD OF THE GROUP), IF AT ALL.    )           17978
      GO TO 4099                                                           17979
 4014 WRITE (LUNIT6, 4114)                                              M17.1929
 4114 FORMAT (5X, 108HFREQUENCY-DEPENDENT REPRESENTATION FOR A MODE OF A   17981
     1 DISTRIBUTED-PARAMETER TRANSMISSION LINE IS REQUESTED BY A   ,/,     17982
     2 5X, 108HVALUE OF  -1  PUNCHED IN FIELD  'IPUNCH'  (COLUMNS 53-54)   17983
     3 OF THE ASSOCIATED BRANCH CARD.   ASSUMING THAT THE    ,/,           17984
     4 5X, 114HREFERENCE-BRANCH PROCEDURE IS NOT USED, THE INPUT OF WEIG   17985
     5HTING FUNCTIONS  A1(T)  AND  A2(T)  FOLLOWS.   THE NUMBER    ,/,     17986
     6 5X, 109HOF POINTS ON THESE INPUT CARDS IS IRRELEVANT, AND IS IN N   17987
     7O WAY RELATED TO THE SIZE OF LIST 14.   INSTEAD, THE     )           17988
      WRITE (LUNIT6, 4214)                                              M17.1930
 4214 FORMAT (5X, 108HLIST-14 STORAGE DEPENDS UPON BOTH THE TIME-SPAN OFM09.1138
     1 THE WEIGHTING FUNCTIONS, AND ALSO UPON THE TIME-STEP SIZE   ,/,     17991
     2 5X, 112H'DELTAT' ,  AS FOLLOWS.   LET  'T1'  BE THE TIME SPAN FRO   17992
     3M THE NONZERO BEGINNING OF  A1(T)  (AT ABOUT ONE TRAVEL    ,/,       17993
     4 5X, 110HTIME) TO WHERE ITS EXPONENTIAL TAIL BEGINS (TYPICALLY ABO   17994
     5UT TWO TRAVEL TIMES).   ALSO, DEFINE  'T2'  TO BE THE    ,/,         17995
     6 5X, 109HTIME AT WHICH THE EXPONENTIAL TAIL OF  A2(T)  BEGINS (TYP   17996
     7ICALLY ABOUT THREE TRAVEL TIMES).   THEN THE STORAGE     )           17997
      WRITE (LUNIT6, 4314)                                              M17.1931
 4314 FORMAT (5X, 102HREQUIREMENT IN LIST 14 IS GIVEN BY THE RELATION   M11. 329
     1  NP = (T1 + T2) / DELTAT  .     LINES WHICH USE THE     ,/,         18000
     2 5X,  60HREFERENCE-BRANCH PROCEDURE REQUIRE NO LIST-14 STORAGE, NO   18001
     3TE.    )                                                             18002
      GO TO 4099                                                           18003
 4015 WRITE (LUNIT6, 4115)                                              M17.1932
 4115 FORMAT (5X, 106HTO PERFORM THE CONVOLUTION ASSOCIATED WITH FREQUEN   18005
     1CY-DEPENDENT MODES OF DISTRIBUTED-PARAMETER TRANSMISSION    ,/,      18006
     2 5X, 112HLINES, MODAL PAST-HISTORY MUST BE STORED FOR BOTH ENDS OF   18007
     3 THE LINE.   WHETHER THE REFERENCE-BRANCH PROCEDURE WAS   ,/,        18008
     4 5X, 109HUSED OR NOT IN NO WAY MODIFIES THIS REQUIREMENT.   FOR EV   18009
     5ERY FREQUENCY-DEPENDENT MODE, TWO CELLS ARE TAKEN UP    ,/,          18010
     6 5X,  11HIN LIST 15.    )                                            18011
      GO TO 4099                                                           18012
 4016 GO TO 4012                                                           18013
 4017 GO TO 4012                                                           18014
 4018 GO TO 4012                                                           18015
 4019 WRITE (LUNIT6, 4119)                                              M23.6430
 4119 FORMAT (5X, 109HDO NOT DISPAIR, ALL IS NOT LOST (YET).   WHAT HAS M23.6431
     1HAPPENED IS THAT LIST  19  IS INADEQUATE FOR THE TACS TABLE  ,/,  M23.6432
     2 5X, 112HSIZES WHICH WERE REQUESTED.   EITHER THE USER SPECIFIED TM23.6433
     3HESE SIZES EXPLICITELY HIMSELF USING AN  'ABSOLUTE TACS     ,/,   M23.6434
     4 5X, 115HDIMENSIONS'  CARD, OR THE EMTP SUPPLIED ITS OWN DEFAULT SM23.6435
     5ET.   IN EITHER CASE, THESE ABSOLUTE TACS TABLE SIZES WILL  ,/,   M23.6436
     6 5X, 114HREQUIRE A LIST-19 SIZE AS SHOWN UNDER THE  'PRESENT FIGURM23.6437
     7E' COLUMN IN ROW  19 .   BEFORE SIMPLY REDIMENSIONING THE   )     M23.6438
      WRITE (LUNIT6, 4219)                                              M23.6439
 4219 FORMAT (5X, 110HEMTP TO PROVIDE SUCH A LIST-19 FIGURE, HOWEVER, THM23.6440
     1E USER MIGHT TRY TO MORE OPTIMALLY DIVIDE THE EXISTING TOTAL ,/,  M23.6441
     2 5X, 115HAMONG THE DIFFERENT TACS TABLES, USING EITHER AN  'ABSOLUM23.6442
     3TE TACS DIMENSIONS'  CARD OR A  'RELATIVE TACS DIMENSIONS'  ,/,   M23.6443
     4 5X, 111HCARD.   FINALLY, BECAUSE   KILL = 122   PROVIDES MUCH GENM23.6444
     5ERAL INFORMATION ABOUT TACS DIMENSIONING, WE SHALL NOW      ,/,   M23.6445
     6 5X,  20HPRINT IT AS WELL....    )                                M23.6446
      KILL = 122                                                        M23.6447
      LSTAT(17) = 0                                                     M23.6448
      LSTAT(16) = 0                                                     M23.6449
      LASTOV = NCHAIN                                                   M23.6450
      NCHAIN = 51                                                       M23.6451
      IF ( IPRSUP  .GE.  1 )                                            M32,2369
     1 WRITE (LUNIT6, 4568)  KILL                                       M39.3913
      GO TO 9000                                                        M39.3914
 4020 GO TO 4012                                                        M15.1884
 4021 GO TO 4012                                                        M15.1885
 4022 GO TO 4012                                                        M16.7996
 4023 GO TO 4012                                                        M16.7997
 4024 N9 = LCOMP * LBUS / NTOT                                          M25.4824
      WRITE (LUNIT6, 4124)  NCOMP, N9                                   M25.4825
 4124 FORMAT (5X,  30HTHE PRESENT DATA CASE INVOLVES,  I4,              M25.4826
     1             36H   PHASE COMPENSATION, WHICH EXCEEDS,             M25.4827
     2             31H THE EFFECTIVE PROGRAM LIMIT OF, I4,              M25.4828
     3              2H .   ,/,                                          M25.4829
     4        5X,  34HTHIS LATTER FIGURE IS THE LIMITING,               M25.4830
     5             31H VALUE OF LIST 24 MULTIPLIED BY,                  M25.4831
     6             30H  LBUS/NTOT  (THE RATIO OF THE     ,/,            M25.4832
     7        5X,  30HMAXIMUM NUMBER OF BUSES TO THE,                   M25.4833
     8             33H ACTUAL NUMBER FOR THIS PROBLEM).   )             M25.4834
      WRITE (LUNIT6, 4224)                                              M25.4835
 4224 FORMAT (5X,  36HNOTE THAT THE EFFECTIVE LIMIT ON THE,             M25.4836
     1             33H NUMBER OF PHASES OF COMPENSATION,                M25.4837
     2             35H THUS VARIES INVERSELY WITH PROBLEM  ,/,          M25.4838
     3        5X,  38HSIZE.  CUT THE SIZE IN HALF, AND TWICE,           M25.4839
     4             38H AS MANY PHASES ARE AVAILABLE, WITHOUT,           M25.4840
     5             38H REDIMENSIONING WITH A LARGER LIST 24.  )         M25.4841
      GO TO 4099                                                        M25.4842
 4025 GO TO 4012                                                        M16.7999
 4026 IF ( LSTAT(13)  .EQ.  0 )   GO TO 4226                            M31.8321
      WRITE (LUNIT6, 4126)  LSTAT(13), LSIZ26                           M31.8322
 4126 FORMAT (5X,  34HTHE USER'S DATA INCLUDES A COUPLED,               M31.8323
     1             16H BRANCH GROUP OF,  I5,                            M31.8324
     2             33H   PHASES.   SQUARING THIS NUMBER,                M31.8325
     3             20H EXCEEDS  LIST 26  (,  I5,  3H ).    )            M31.8326
      GO TO 4099                                                        M31.8327
 4226 WRITE (LUNIT6, 4326)                                              M31.8328
 4326 FORMAT (5X,  40HLIST 26 WORKING VECTORS "VOLT", "VOLTI",,         M31.8329
     1             38H "VOLTK", "VIM", AND "VOLTA"  ARE USED,           M31.8330
     2             32H IN VARIOUS WAYS.   THIS IS ONE.     ,/,          M31.8331
     3        5X,  36HIF USER DOES NOT REQUEST 50 OR MORE,,             M31.8332
     4             29H  AND IF HE HAS TROUBLE,  SEE,                    M31.8333
     5             33H PROGRAM MAINTENANCE FOR DETAILS.    )            M31.8334
      GO TO 4099                                                        M31.8335
 4027 GO TO 4012                                                        M16.8001
 4028 GO TO 4012                                                        M16.8002
 4029 GO TO 4012                                                        M16.8003
 4499 WRITE (LUNIT6, 4199)                                              M17.1933
 4199 FORMAT (5X, 109HBOTH NETWORK NODE-RENUMBERING (TRANSIENT AND ALSO    18017
     1STEADY-STATE) AND THE STEADY-STATE PHASOR SOLUTION MAKE USE   ,/,    18018
     2 5X, 113HOF THREE VERY LARGE ARRAYS WHICH OVERLAY MOST OF THE LABE   18019
     3LED-COMMON STORAGE SPACE (THE DATA OF WHICH IS PRESERVED    ,/,      18020
     4 5X, 111HON LOGICAL 4 DURING THESE CALCULATIONS).   THIS IS A DYNA   18021
     5MICALLY-DIMENSIONED TABLE, THEN, WHICH IS SIZED TO USE   ,/,         18022
     6 5X, 107HALL AVAILABLE SPACE (PERHAPS 2/3 OF LABELED COMMON).   IN   18023
     7 PARTICULAR, THIS WORKING AREA INCLUDES ALL OF THE    )              18024
      WRITE (LUNIT6, 4299)                                              M17.1934
 4299 FORMAT (5X, 113HGENERALLY-LARGE STORAGE FOR LISTS 5 AND 8.   INCRE   18026
     1ASING THE DIMENSIONS OF EITHER OF THESE TWO LISTS WILL DIRECTLY,/,   18027
     2 5X, 105H(AND WITHOUT ANY LOSS) INCREASE THE SIZE OF LIST 99.   IT   18028
     3 MIGHT BE MENTIONED THAT THE STEADY-STATE PHASOR    ,/,              18029
     4 5X, 107HMANIPULATIONS (RENUMBERING, SOLUTION) WILL ALMOST ALWAYS    18030
     5PROVIDE THE LIMITING DIFFICULTY.   THIS IS BECAUSE    ,/,            18031
     6 5X, 110HSPARSITY OF THE STEADY-STATE PHASOR NETWORK IS GENERALLY    18032
     7WORSE THAN FOR THE TIME-STEP-LOOP NETWORK, DUE TO THE    )           18033
      WRITE (LUNIT6, 4399)                                              M17.1935
 4399 FORMAT (5X, 104HDIFFERENCE IN TREATMENT OF DISTRIBUTED-PARAMETER L   18035
     1INES.   FOR STEADY-STATE SOLUTION, EQUIVALENT BRANCHES   ,/,         18036
     2 5X, 110HINTERCONNECT EVERY TERMINAL NODE OF THE LINE, WHILE THE T   18037
     3WO ENDS ARE DISCONNECTED BY BERGERON'S METHOD FOR THE   ,/,          18038
     4 5X, 110HTIME-STEP-LOOP NETWORK.   DOUBLE-CIRCUIT (6-CONDUCTOR) LI   18039
     5NES ARE PARTICULARLY NASTY IN THE STEADY-STATE, THEN,    ,/,         18040
     6 5X,  77HHAVING 12 TERMINAL NODES WHICH ARE ALL INTERCONNECTED BY    18041
     7EQUIVALENT BRANCHES.    )                                            18042
 4099 WRITE (LUNIT6, 4098)                                              M17.1936
 4098 FORMAT ( /, 107H IN ORDER TO EFFECTIVELY TRADE MEMORY SPACE AMONG    18044
     1THE DIFFERENT TABLES, ONE MUST KNOW HOW MANY ARRAYS THERE   ,/,      18045
     2 114H ARE IN EACH TABLE (EFFECTIVELY).   THE FOLLOWING TABULATION    18046
     3SHOWS THE EFFECTIVE MULTIPLICITY ASSOCIATED WITH EACH    ,/,         18047
     4 113H INDEPENDENT LIST ----- THOSE LISTS WHOSE LENGTHS ARE UNDER U   18048
     5SER  CONTROL BY MEANS OF EMTP VARIABLE DIMENSIONING.   )          M27, 179
      WRITE (LUNIT6, 4096)                                              M17.1937
 4096 FORMAT ( 5X,  30H-------------1----------------,                  M35.9759
     1              30H------------------------------,                  M27, 181
     2              30H------------------------------,                  M27, 182
     3              24H------------------------    )                    M27, 183
      WRITE (LUNIT6, 4095)  ( I, I=1, 25 )                              M27, 184
 4095 FORMAT ( 5X, 14HLIST NUMBER  1, 25I4 )                            M14.4679
      WRITE (LUNIT6, 4096)                                              M17.1940
      WRITE (LUNIT6, 4093)                                              M17.1942
 4093 FORMAT ( 5X, 14HFLOATING PT. 1,                                      18060
     1100H   6   5   3   6   1  12   2   2   8   3   1   4   8   1   2  M27, 185
     2 0   6   1   1  24   2   1   #   *   1     )                      M27, 186
      WRITE (LUNIT6, 4097)                                              M17.1943
 4097 FORMAT(5X, 14HINTEGER      1,                                        18064
     1100H   4   7   0   2   1  10   0   0  11   0   3   0   4   0   0  M27, 187
     2 1  10   2   0   0   0   0   0   0   0       ,/,                  M27, 188
     2  5X,  14HTOTAL        1,                                         M27, 189
     3100H  10  12   3   8   2  22   2   2  19   3   4   4  12   1   2  M27, 190
     4 1  16   3   1  24   2   1   #   *   1       )                    M27, 191
      WRITE (LUNIT6, 4096)                                              M17.1944
      WRITE (LUNIT6, 4091)                                              M27, 192
 4091 FORMAT ( 3X,  36H# --- USED ONLY FOR VIRTUAL MACHINES,            M37.9182
     1              38H (BURROUGHS, PRIME, VAX, APOLLO, ETC.),          M37.9183
     2              31H   OTHERS CAN IGNORE THIS LIST.   ,/,            M37.9184
     3         3X,  31H* --- RATHER THAN COUNT LIST 24,                 M27, 196
     4              29H ITSELF, ADD THE VALUE TO THE,                   M27, 197
     5              32H FLOATING-POINT AND TOTAL COUNTS,                M27, 198
     6              19H FOR LISTS 1 AND 6.    )                         M27, 199
 4092 WRITE(LUNIT6, 5315)                                               M39.3915
 5315 FORMAT( 1H  )                                                        18074
      WRITE (LUNIT6, 5314)                                              M19.3714
 5314 FORMAT ( 25X,   97HCAUTION.   BE SKEPTICAL OF ABOVE  'PRESENT FIGUM19.3715
     1RE'  ENTRIES, DUE TO ABNORMAL TERMINATION OF CASE.     )          M19.3716
      WRITE(LUNIT6, 5316)                                               M17.1947
 5316 FORMAT( 132H -----------------------------------------------------   18076
     1------------------------------------------------------------------   18077
     2------------     )                                                   18078
      DO 6512  I=1, 2                                                      18079
 6512 WRITE(LUNIT6, 5320)                                               M17.1948
 5320 FORMAT( 132H ERROR/ERROR/ERROR/ERROR/ERROR/ERROR/ERROR/ERROR/ERROR   18081
     1/ERROR/ERROR/ERROR/ERROR/ERROR/ERROR/ERROR/ERROR/ERROR/ERROR/ERROR   18082
     2/ERROR/ERROR     )                                                   18083
      WRITE(LUNIT6, 5316)                                               M17.1949
      IF ( KILSAV  .EQ.  92 )    GO TO 6767                                18085
C     LUNIT5 = MTAPE                                                    M39.3916
      WRITE (*,*)  ' COMMENTED OUT  LUNIT5 = MTAPE.',                   M39.3917
     1             '   SET KILL = 0.'                                   M39.3918
      KILL = 0                                                          M39.3919
      N6 = 0                                                            M23.6453
C     READ INPUT CARD USING CIMAGE                                         18086
 6740 CALL CIMAGE                                                          18087
      N13 = KOLBEG                                                      M23.6455
      NRIGHT = -2                                                       M22.7436
      KOLBEG = 1                                                        M22.7437
      DECODE (80, 6741, ABUFF(1) )   TEXCOL                             M22.7438
 6741 FORMAT ( 80A1 )                                                   M22.7439
      CALL FREONE ( D1 )                                                M29.6244
      NRIGHT = 0                                                        M22.7441
      IF ( NFRFLD  .NE.  1 )   GO TO 6743                               M22.7442
      IF ( TEXTA6(1)  .EQ.  TEXT5 )   GO TO 6754                        M22.7443
      IF ( TEXTA6(1)  .EQ.  TEXT13 )   GO TO 6764                       M22.7444
      IF ( TEXTA6(1)  .EQ.  TEXT19 )   GO TO 6771                       M23.6456
 6743 IF ( NFRFLD  .NE.  4 )   GO TO 6770                               M22.7445
      IF ( TEXTA6(4)  .NE.  TEXT4 )   GO TO 6770                        M22.7446
      IF ( TEXTA6(3)  .NE.  TEXT3 )   GO TO 6770                        M22.7447
      IF ( TEXTA6(2)  .NE.  TEXT2 )   GO TO 6760                        M22.7448
      IF ( TEXTA6(1)  .NE.  TEXT1 )   GO TO 6760                        M22.7449
 6754 WRITE (KUNIT6, 6755)                                              M35.9760
 6755 FORMAT(  37H+MARKER CARD PRECEDING NEW DATA CASE.    )               18096
      CALL INTERP                                                       M35.9761
      IF ( N6  .LT.  5 )   GO TO 6758                                   M23.6457
      N6 = N6 - 4                                                       M23.6458
      WRITE (LUNIT6, 6757)  N6                                          M23.6459
 6757 FORMAT ( 51X,  27HEND SUPPRESSION OF LISTING.,  I6,               M23.6460
     1  22H  CARDS WERE UNLISTED.  )                                    M23.6461
 6758 NOUTPR = 0                                                        M23.6462
      KOLBEG = -INTINF                                                  M23.6464
      LASTOV = NCHAIN                                                   M15.1897
      NCHAIN = 1                                                           18097
      IF ( IPRSUP  .GE.  1 )                                            M32,2371
     1 WRITE ( LUNIT6, 4568)  4568                                      M39.3920
      GO TO 9000                                                        M39.3921
 6760 IF ( TEXTA6(2)  .NE.  TEXT12 )   GO TO 6770                       M22.7453
      IF ( TEXTA6(1)  .NE.  TEXT11 )   GO TO 6770                       M22.7454
 6764 WRITE (KUNIT6, 6765)                                              M35.9762
 6765 FORMAT(  38H+MARKER CARD FOLLOWING LAST DATA CASE.   )               18105
      CALL INTERP                                                       M35.9763
 6767 KILL = 9999                                                          18106
      LASTOV = NCHAIN                                                   M15.1898
      NCHAIN = 31                                                          18107
      IF ( IPRSUP  .GE.  1 )                                            M32,2374
     1 WRITE ( LUNIT6, 4568)  KILL                                      M39.3922
      GO TO 9000                                                        M39.3923
 6770 IF ( NFRFLD  .NE.  5 )   GO TO 6773                               M23.6465
      IF ( TEXTA6(1)  .NE.  TEXT14 )   GO TO 6773                       M23.6466
      IF ( TEXTA6(2)  .NE.  TEXT15 )   GO TO 6773                       M23.6467
      IF ( TEXTA6(3)  .NE.  TEXT16 )   GO TO 6773                       M23.6468
      IF ( TEXTA6(4)  .NE.  TEXT17 )   GO TO 6773                       M23.6469
      IF ( TEXTA6(5)  .NE.  TEXT18 )   GO TO 6773                       M23.6470
 6771 IF ( NENERG  .EQ.  0 )   GO TO 6773                               M23.6471
      IF ( KNT     .LE.  1 )   GO TO 6773                               M23.6472
      WRITE (KUNIT6, 6772)                                              M35.9764
 6772 FORMAT ( 32H+REQUEST FOR STATISTICS SALVAGE.  )                   M23.6474
      CALL INTERP                                                       M35.9765
      IF ( JFLSOS  .GT.  0 )   GO TO 6773                               M23.6475
      D7 = -9999.                                                       M23.6476
      N15 = KSWTCH + 1                                                  M23.6477
      WRITE (LUNIT3)  ( D7, J=1, N15 )                                  M23.6478
      N15 = LSTAT(32)                                                   M23.6479
      WRITE (LUNIT9)  ( D7, J=1, N15 )                                  M23.6480
      IF ( N13  .EQ.  -INTINF )   KOLBEG = N13                          M23.6481
      IF ( KOLBEG  .GT.  0 )   GO TO 1773                               M23.6482
      DECODE (80, 1764, ABUFF(1) )   N1                                 M23.6483
 1764 FORMAT ( 29X, I3 )                                                M23.6484
      GO TO 1778                                                        M23.6485
 1773 CALL FREONE ( D1 )                                                M29.6245
      N1 = D1                                                           M23.6487
 1778 IF ( N1  .GT.  0 )   GO TO 1774                                   M23.6488
      IF ( N1  .LT.  0 )   IPRSUP = 9                                   M23.6489
C     FIND RANDOM INTEGER  'N1'  BETWEEN ZERO AND 999.                  M23.6490
      CALL RUNTYM ( D11, D12 )                                          M23.6491
      SEED = SEEDY( TCLOCK(1) )  +  1000. * ( D11 + D12 )               M23.6492
      N13 = ALOG1Z(SEED)  +  EPSILN                                     M23.6493
      N13 = N13 - 2                                                     M23.6494
      SEED = SEED / 10.**N13                                            M23.6495
      N1 = SEED                                                         M23.6496
 1774 N2 = N1 / 100                                                     M23.6497
      N5 = N1 - 100 * N2                                                M23.6498
      N3 = N5 / 10                                                      M23.6499
      N4 = N5 - 10 * N3                                                 M23.6500
      WRITE(LUNIT6, 1792)  N2, N3, N4                                   M23.6501
 1792 FORMAT ( 1H+,  34X,  1H',  3I1,  1H'  )                           M23.6502
      LSTAT(14) = N2                                                    M23.6503
      LSTAT(15) = N3                                                    M23.6504
      LSTAT(16) = N4                                                    M23.6505
      CALL STATSV                                                       M23.6506
      GO TO 6740                                                        M23.6507
 6773 IF ( NOUTPR  .EQ.  0 )                                            M23.6508
     1 WRITE (KUNIT6, 6775)                                             M35.9766
 6775 FORMAT (   47H+CARD IGNORED IN SEARCH FOR NEW-CASE BEGINNING.   )    18110
      N6 = N6 + 1                                                       M23.6510
      IF ( N6  .LT.  5 )   GO TO 6769                                   M23.6511
      IF ( NOUTPR  .EQ.  1 )   GO TO 6769                               M23.6512
      NOUTPR = 1                                                        M23.6513
      WRITE (LUNIT6, 6768)                                              M23.6514
 6768 FORMAT ( 51X, 45HBEGIN SUPPRESSION OF SKIPPED-RECORD PRINTOUT. )  M23.6515
 6769 GO TO 6740                                                        M23.6516
 9000 RETURN                                                            M39.3924
      END                                                                  18154
      SUBROUTINE STATSV                                                 M23.6518
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      RETURN                                                            M23.6567
      END                                                               M23.6568
C  M99.9999      OVERLAY ( FINISH, 99999 )                                         
