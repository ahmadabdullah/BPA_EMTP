      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'VOLT45.FOR;/NOLIST'                                           
C)**********************************************************************M15. 203
C)                                                                     *M15. 204
C)    --------------- ELECTROMAGNETIC TRANSIENTS PROGRAM ------------  *M15. 205
C)                    METHODS DEVELOPMENT BRANCH, ROUTE EOGB           *M38.  87
C)                    DIVISION OF SYSTEM ENGINEERING                   *M38.  88
C)                    BONNEVILLE POWER ADMINISTRATION                  *M15. 208
C)                    P. O. BOX 3621                                   *M15. 209
C)                    PORTLAND, OREGON  97208                          *M15. 210
C)                    U.S.A.     PHONE: (503) 230-4404                 *M38.  89
C)                                                                     *M15. 212
C)    THE FORTRAN COMMENT-CARD TEXT NOW BEING READ REPRESENTS A        *M15. 213
C)    SUMMARY INTRODUCTION AND EXPLANATION WHICH APPLIES TO A VERY     *M15. 214
C)    LARGE PROGRAM DEVELOPMENT REFERRED TO BY THE TITLE OF            *M15. 215
C)    'ELECTROMAGNETIC TRANSIENTS PROGRAM'  (ABBREVIATED  'EMTP' ,     *M28. 115
C)    OR   'T.P.'  IN THE OLDER NOTATION).                             *M28. 116
C)                                                                     *M15. 218
C)    IN GENERAL TERMS, THE PURPOSE OF THIS WORK IS TO SIMULATE        *M15. 219
C)    TRANSIENT PHENOMENA IN POWER SYSTEM NETWORKS, TAKING INTO        *M15. 220
C)    ACCOUNT TRAVELING WAVES (ELECTROMAGNETIC TRANSIENTS) ON          *M15. 221
C)    TRANSMISSION LINES, SATURATION OF TRANSFORMERS, NONLINEARITIES   *M15. 222
C)    OF SURGE ARRESTERS, ETC.   WHILE NOT SO RESTRICTED IN THEORY,    *M15. 223
C)    THE MOST COMMON PROGRAM APPLICATION IS FOR THE SIMULATION OF     *M15. 224
C)    SWITCHING SURGES WHICH PROPAGATE ON POWER NETWORK TRANSMISSION   *M15. 225
C)    LINES.   FOR A MORE DETAILED EXPLANTION OF THE MODELING          *M15. 226
C)    TECHNIQUES WHICH ARE USED, THE READER IS REFERRED TO THE         *M28. 117
C)    MANUAL FOR THIS PROGRAM (840 PAGES FOR THE VERSION DATED         *M36.  56
C)    MARCH, 1983).  ).    WHILE OLDER ISSUES WERE TITLED              *M36.  57
C)    "EMTP USER'S MANUAL",  BEGINNING IN SEPTEMBER OF 1980            *M28. 120
C)    THIS WORK IS NOW CALLED THE  "EMTP RULE BOOK" .                  *M28. 121
C)                                                                     *M15. 229
C     THE UTPF IS A LARGE 80-COLUMN BCD CARD-IMAGE FILE, TO BE USED    *M03. 814
C     AS INPUT TO E/T PROGRAMS.   E/T PROGRAMS MACHINE TRANSLATE THIS  *M03. 815
C     UTPF CODE INTO LEGAL FORTRAN FOR MOST COMPUTER SYSTEMS OF        *M28. 125
C     INTEREST (IBM, CDC, UNIVAC, HONEYWELL, DEC PDP-10, DEC VAX-11,   *M28. 126
C     PRIME, SEL, APOLLO, HITACHI, FACOM, HARRIS, ETC.).               *VAX.   7
C                                                                      *M03. 824
C     IN CONFORMITY WITH LONG-STANDING BPA POLICY, AS WELL AS THE      *M03. 825
C)    MORE RECENT (FEBRUARY 19, 1975) FEDERAL FREEDOM OF INFORMATION   *M15. 231
C)    ACT, DISSEMINATION OF THESE PROGRAM MATERIALS IS FREELY MADE     *M15. 232
C)    TO ANY AND ALL INTERESTED PARTIES.   A FEE TO COVER REPRODUCTION,*M15. 233
C)    HANDLING, AND MAILING COSTS MAY BE ASSESSED AGAINST THE          *M15. 234
C)    ORGANIZATION RECEIVING THE MATERIAL, HOWEVER.   NO CLAIM OR      *M15. 235
C)    WARRANTY AS TO THE USEFULNESS, ACCURACY, FIDELITY, OR            *M15. 236
C)    COMPLETENESS OF THESE MATERIALS IS (OR EVER HAS BEEN) IN ANY     *M15. 237
C)    WAY EXPRESSED OR IMPLIED.                                        *M15. 238
C                                                                      *M36.  58
C)**********************************************************************M15. 240
      DATA  LL34   /  34  /                                             M36.  68
C     UNIT ASSIGNMENTS OF "OVER1" NEEDED EARLIER BY SPY:                M38.  90
      LUNIT1 = 1                                                        M38.  91
      LUNIT2 = 2                                                        M38.  92
      LUNIT3 = 3                                                        M38.  93
      LUNIT4 = 4                                                        M38.  94
      LUNIT5 = 5                                                        M38.  95
      LUNIT6 = 6                                                        M38.  96
      LUNIT7 = 7                                                        M38.  97
      LUNIT8 = 8                                                        M38.  98
      LUNIT9 = 9                                                        M38.  99
      LUNT10 = 10                                                       M38. 100
      LUNT11 = 11                                                       M38. 101
      LUNT12 = 12                                                       M38. 102
      LUNT13 = 13                                                       M38. 103
      LUNT14 = 14                                                       M38. 104
      LUNT15 = 15                                                       M38. 105
      LLBUFF = -3333                                                    M06.   9
      KOL132 = 132                                                      M32. 122
      NCHAIN = -1                                                               
      LASTOV = 0                                                        M20. 186
      KILL = 0                                                          M22. 623
 2000 IF ( KILL  .EQ.  0 )   GO TO 2001                                 M22. 624
      IF ( KILL  .EQ.  9999 )   GO TO 2001                              M22. 625
      IF ( KILL .NE. 7733 )  GO TO 4372                                         
      WRITE (LUNIT6, 4367)                                              M36.  71
 4367 FORMAT ( 39H "MAIN00" INTERCEPT OF "BEGIN" REQUEST.  )            M36.  72
      KILL = 0                                                                  
      NUMDCD = 0                                                                
      NCHAIN = 1                                                                
      GO TO 1983                                                                
 4372 IF ( NCHAIN  .GT.  51 )   GO TO 2001                              M35. 192
      NCHAIN = 51                                                       M22. 627
 2001 N1 = NCHAIN                                                       M22. 628
      IF ( N1  .GT.  30 )   N1 = N1 - 30                                M13. 175
      IF ( N1  .LE.  0 )  N1 = 1                                        M36.  73
      IPRSUP = IPRSOV(N1)                                               M13. 176
      IF (NCHAIN .GT. 20)   GO TO 2010                                  M13. 177
      IF ( NCHAIN .EQ. 12 .OR. NCHAIN .EQ. 2  )  GO TO 1983                                         
      IF ( NCHAIN .EQ. -1 )                                                     
     1 CALL MOVE0 ( IPRSOV(1), LL34 )                                           
      CALL EREXIT                                                               
      NCHAIN = 0                                                                
      IF ( NCHAIN  .GT.  20 )   GO TO 2000                              M36.  77
 1983 CALL MAIN10
      GO TO 2000                                                        M42.  15
 2010 IF ( M4PLOT .EQ. 1 )  CALL EMTSPY                                         
      IF ( NCHAIN  .GT.  29 )   GO TO 2020                              M37. 123
      CALL OVER29
      GO TO 2000                                                        M42.  17
 2020 IF ( NCHAIN  .GT.  31 )   GO TO 2024                              M32. 123
      CALL OVER31
      GO TO 2000                                                        M42.  18
 2024 IF ( NCHAIN  .GT.  39 )   GO TO 2025                              M32. 124
      CALL OVER39
      GO TO 2000                                                        M32. 126
 2025 IF ( NCHAIN  .GT.  41 )   GO TO 2070                              M36.  78
      CALL OVER41
      GO TO 2000                                                        M42.  20
 2070 IF ( NCHAIN  .GT.  42 )   GO TO 2080                              M42.  21
      CALL OVER42
      GO TO 2000                                                        M42.  23
 2080 IF (NCHAIN .GT. 44) GO TO 2100                                    M42.  24
      CALL OVER44
      GO TO 2000                                                        M42.  28
 2100 IF ( NCHAIN  .GT.  45 )   GO TO 2110                              M15. 241
      CALL OVER45
      GO TO 2000                                                        M14. 228
 2110 IF ( NCHAIN  .GT.  47 )   GO TO 2130                              VAX.   8
      CALL OVER47
      GO TO 2000                                                        M15. 248
 2130 IF ( NCHAIN  .GT.  51 )   GO TO 2260                              VAX.   9
      CALL OVER51
      GO TO 2000                                                        M42.  31
 2260 IF ( NCHAIN  .GT.  52 )   GO TO 2270                              M42.  32
      CALL OVER52
      GO TO 2000                                                        M42.  34
 2270 IF ( NCHAIN  .GT.  53 )   GO TO 2280                              M15. 249
      CALL OVER53
      GO TO 2000                                                        M42.  35
 2280 IF ( NCHAIN  .GT.  54 )   GO TO 2290                              M15. 251
      CALL OVER54
      GO TO 2000                                                        M15. 253
 2290 IF ( NCHAIN  .GT.  55 )   GO TO 2300                              M25.  50
      CALL OVER55
      GO TO 2000                                                        M25.  52
 2300 WRITE (LUNIT6, 9236)  NCHAIN                                      M37. 124
 9236 FORMAT ( /,  26H ILLEGAL NCHAIN IN MAIN00., I8 )                  M37. 125
      GO TO 2000                                                                
C)                                                                      M15. 258
C)    THE PRESENT MODULE  MAIN00  IS ALWAYS IN MEMORY.   IT IS THE      VAX.  10
C)    HIGHEST LEVEL MODULE OF A PROGRAM WHICH HAS TWO LEVELS OF         M15. 260
C)    OVERLAYING.   IT CALLS PRIMARY LEVEL OVERLAYS ONLY (DIRECTLY),    M15. 261
C)    BASED ON THE VALUE OF VARIABLE  'NCHAIN' .   THE FOLLOWING        M15. 262
C     LEGITIMATE VALUES, AND THE MEANING OF THE ASSOCIATED OVERLAY      M42.  36
C     CALLS, EXIST .....                                                M42.  37
C     1-20.  FOR OVERLAYS 1, 2, ..., 20, WHICH ARE SECONDARY-LEVEL      M42.  38
C            OVERLAYS, A CALL MUST BE FIRST MADE TO THE CONTROLLING     M42.  39
C            PRIMARY-LEVEL OVERLAY.   THUS FOR SUCH  'NCHAIN'  VALUES,  M42.  40
C            CONTROL IS TRANSFERED FIRST TO MODULE  MAIN10 .   THIS     M42.  41
C            IS THE ONLY CASE WHERE CALLS TO OVERLAYS ARE NOT MADE      M42.  42
C            DIRECTLY.                                                  M42.  43
C                                                                       M42.  44
C     29.  COMPLETION OF STATISTICS (MONTE CARLO) STUDY, WHERE VARIABLE M42.  45
C          MAXIMA OF THE DIFFERENT CASE SOLUTIONS ARE READ OFF THE      M42.  46
C          DISK, AND ARE PROCESSED STATISTICALLY TO PRODUCE             M42.  47
C          CUMULATIVE DISTRIBUTION FUNCTIONS, ETC.                      M42.  48
C                                                                       M42.  49
C     31.  PLOT ROUTINE, FOR GRAPHICAL OUTPUT OF TRANSIENTS.            M42.  50
C          THE PROGRAM ALSO TERMINATES EXECUTION HERE, USUALLY,         M37. 127
C          AFTER WRITING AN END-OF-INFORMATION MARK ON THE              M37. 128
C          PLOT TAPE (WHETHER OR NOT THE USER HAS PLOTTED ANYTHING).    M42.  51
C                                                                       M42.  52
C     39.  SUPPORTING ROUTINE WHICH GENERATES EMTP BRANCH               M36.  81
C          CARDS FOR THE FREQUENCY-DEPENDENT REPRESENTATION OF          M29.  70
C          AN UNTRANSPOSED TRANSMISSION LINE.   THIS IS THE             M29.  71
C          "MARTI SETUP"  CODE, NAMED AFTER DR. JOSE MARTI OF           M29.  72
C          VANCOUVER AND CARACAS (SEE 1981 IEEE PICA PAPER).            M29.  73
C                                                                       M42.  53
C     41.  SUPPORTING ROUTINE WHICH CALCULATES TRANSFORMER MATRICES  (R)M42.  54
C          AND  (L)  FROM SHORT-CIRCUIT AND OPEN-CIRCUIT DATA.          M42.  55
C                                                                       M29.  74
C     42.  SUPPORTING ROUTINE WHICH CONVERTS AN RMS VOLTAGE VS. CURRENT M42.  56
C          SATURATION CHARACTERISTIC INTO AN INSTANTANEOUS FLUX VS.     M42.  57
C          CURRENT CHARACTERISTIC.                                      M42.  58
C                                                                       M29.  75
C     43.  SUPPORTING ROUTINE WHICH CALCULATES WEIGHTING FUNCTIONS      M42.  59
C          A1(T)  AND  A2(2)  FOR THE ZERO-SEQUENCE MODE OF A           M42.  60
C          DISTRIBUTED LINE WHICH HAS FREQUENCY-DEPENDENT LINE          M42.  61
C          CONSTANTS  R  AND  L .                                       M42.  62
C                                                                       M42.  63
C     44.  SUPPORTING ROUTINE WHICH CALCULATES LINE CONSTANTS FOR       M42.  64
C          OVERHEAD TRANSMISSION LINES BY MEANS OF CARSON'S FORMULA.    M42.  65
C          THIS IS A MODIFIED VERSION OF WHAT WAS ORIGINALLY (UNTIL     M42.  66
C          JANUARY 1975) THE COMPLETELY-SEPARATE BPA LINE-CONSTANTS     M42.  67
C          PROGRAM.                                                     M42.  68
C                                                                       M42.  69
C     45.  SUPPORTING ROUTINE OF  'SEMLYEN SETUP'  CODE.   THE OUTPUT   M19. 193
C          IS A GROUP OF PUNCHED CARDS, AS ARE REQUIRED FOR THE EMTP    M19. 194
C          SIMULATION OF A TRANSMISSION CIRCUIT USING  SEMLYEN          M19. 195
C          RECURSIVE CONVOLUTION MODELING.                              M19. 196
C                                                                       M29.  76
C     47.  SUPPORTING ROUTINE OF  'CABLE CONSTANTS'  CODE.   THE        M19. 201
C          PRIMARY FUNCTION IS TO CALCULATE  (R),  (L),  %  (C)         M19. 202
C          MATRICES FOR A MULTI-PHASE SYSTEM OF SINGLE-CORE COAXIAL     M19. 203
C          CABLES.                                                      M19. 204
C                                                                       M42.  70
C     51.  PRINTING OF INTRODUCTORY PARAGRAPH OF ERROR-MESSAGE          M42.  73
C          TERMINATION ('YOU LOSE, FELLA, ... '), PLUS ERROR-MESSAGE    M42.  74
C          TEXTS FOR  'KILL'  CODES NUMBERED  1  THROUGH  50 .          M42.  75
C          THE EXITING LINKAGE IS TO THE LAST ERROR OVERLAY.            M25.  59
C     52.  ERROR MESSAGE TEXTS FOR  'KILL'  CODES NUMBERED  51          M42.  76
C          THE EXITING LINKAGE IS TO THE LAST ERROR OVERLAY.            M25.  60
C     53.  ERROR MESSAGE TEXTS FOR  'KILL'  CODES NUMBERED  91          M15. 263
C          THROUGH  150.  THE EXITING LINKAGE IS TO THE LAST            M25.  61
C          ERROR OVERLAY.                                               M25.  62
C     54.  ERROR MESSAGE TEXTS FOR  'KILL'  CODES NUMBERED  151         M15. 265
C          THROUGH  200.   THE EXITING LINKAGE IS TO THE                M25.  63
C          LAST ERROR OVERLAY.                                          M25.  64
C     55.  FINAL ERROR OVERLAY.  MESSAGES FOR  KILL = 201               M25.  65
C          ONWARD ARE CONTAINED, AS WELL AS SUMMARY STATISTICS          M25.  66
C          --- TABLE SIZES AND TIMING FIGURES FOR THE RUN.              M25.  67
C          THE EXITING LINKAGE IS GENERALLY TO MODULE  OVER1  (TO READ  M42.  77
C          A NEW DATA CASE), BUT MAY BE TO MODULE  OVER31 (FOR FINAL    M42.  78
C          CASE TERMINATION).                                           M42.  79
      END                                                               M42.  80
      SUBROUTINE STOPTP                                                 M37. 129
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     TEMPORARY STOP STATEMENTS OF EMTP HAVE BEEN CONVERTED TO          M37. 130
C     "CALL STOPTP", ALLOWING INSTALLATION-DEPENDENT CLEAN UP.          M37. 131
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      DECODE (80, 5607, ABUFF)  TEXCOL                                  M37. 133
 5607 FORMAT ( 80A1 )                                                   M37. 134
      IF ( NCHAIN .EQ. 31  .AND.  LASTOV .EQ. 1  .AND.                  M38. 108
     1     KILL .EQ. 9999 )  GO TO 9000                                 M38. 109
      WRITE (LUNIT6, 5623)  NCHAIN, LASTOV, TEXCOL                      M37. 135
 5623 FORMAT ( /, 34H TEMPORARY ERROR STOP IN "STOPTP".,                M37. 136
     1            19H   NCHAIN, LASTOV =,  2I5,  5X,                    M37. 137
     2            39HLAST-READ CARD IMAGE ABUFF FOLLOWS ....            M37. 138
     3        ,/,  80A1  )                                              M37. 139
 9000 STOP                                                              M38. 110
      END                                                               M37. 141
      SUBROUTINE COPYR ( D1, TO, KK )                                   M22. 637
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     ROUTINE WHICH COPIES THE SAME FLOATING-POINT WORD  'D1'  INTO A   M22. 638
C     CONTIGUOUS REGION OF MEMORY ----  'KK'  WORDS IN LENGTH,          M22. 639
C     BEGINNING WITH WORD  TO(1) .                                      M22. 640
      DIMENSION  TO(1)                                                  M22. 641
      DO 5431  I=1, KK                                                  M22. 642
 5431 TO(I) = D1                                                        M22. 643
      RETURN                                                            M22. 644
      END                                                               M22. 645
      SUBROUTINE COPYI ( N1, ITO, KK )                                  M22. 646
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     ROUTINE WHICH COPIES THE SAME INTEGER WORD  'N1'  INTO A          M22. 647
C     CONTIGUOUS REGION OF MEMORY ----  'KK'  WORDS IN LENGTH,          M22. 648
C     BEGINNING WITH WORD  ITO(1) .                                     M22. 649
      DIMENSION  ITO(1)                                                 M22. 650
      DO 5465  I=1, KK                                                  M22. 651
 5465 ITO(I) = N1                                                       M22. 652
      RETURN                                                            M22. 653
      END                                                               M22. 654
      SUBROUTINE COPYA ( TEXT1, TEXT2, KK )                             M23.  30
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     ROUTINE WHICH COPIES THE SAME ALPHANUMERIC WORD  'TEXT1'  INTO    M23.  31
C     A CONTIGUOUS REGION OF MEMORY ----  'KK'  WORDS IN LENGTH,        M23.  32
C     BEGINNING WITH WORD  TEXT2(1) .                                   M23.  33
      REAL*8         TEXT1, TEXT2                                       M23.  34
      DIMENSION  TEXT2(1)                                               M23.  35
      DO 5483  I=1, KK                                                  M23.  36
 5483 TEXT2(I) = TEXT1                                                  M23.  37
      RETURN                                                            M23.  38
      END                                                               M23.  39
      SUBROUTINE EREXIT                                                 M37. 142
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C       VAX-11   INSTALLATION-DEPENDENT EMTP MODULE.   THIS IS          M37. 143
C     CALLED BY THE TOP OF "MAIN00", BEFORE ANY EMTP DATA INPUT.        M37. 144
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      DIMENSION  IDUM(3)   !  DUMMY VECTOR FOR CTRL-C HANDLING          M38. 112
      EXTERNAL KWITER       ! NEEDED FOR CTRL-C INITIALIZATION          M38. 113
      COMMON /COMKWT/  KWTVAX  ! MAGIC BLOCK FOR VAX/VMS CTRL-C         M37. 146
      LUNIT6 = 6  ! FOR USE OF "PROMPT" UNTIL FIXED TT?? ADDRESS        M37. 147
      MUNTSV(2) = 49  ! ALTERNATE MUNIT5 UNIT NUMBER OF SPY             M37. 148
      KWTVAX = 0    ! SET FLAG CORRESPONDING TO NO CTRL-C USAGE         M38. 114
C      CALL ENABLE_CTRL_C ( KWITER, IDUM(1) )  ! INITIALIZE CTRL-C       M38. 115
      CALL DATAIN    ! READ DATA IN, PROCESS $INCLUDE, SPY, ETC.        M37. 152
      RETURN                                                            M37. 153
      END                                                               M37. 154
      SUBROUTINE RUNTYM ( D1, D2 )                                      M40.  81
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    THIS SUBROUTINE RETURNS WITH THE CURRENT JOB-EXECUTION TIME, AS   M15. 266
C)    BROKEN DOWN INTO TWO CATEGORIES ....                              M15. 267
C)           D1 = CENTRAL PROCESSOR JOB TIME, IN SECONDS                M15. 268
C)           D2 = PERIPHERAL PROCESSOR (OR INPUT/OUTPUT) JOB TIME,      M15. 269
C)                IN SECONDS.                                           M15. 270
C)    IF TWO SUCH FIGURES ARE NOT AVAILABLE ON THE USER'S COMPUTER,     M15. 271
C)    'D2'  SHOULD BE SET TO ZERO SO THAT CASE-SUMMARY STATISTICS       M15. 272
C)    WILL NOT PRINT OUT GARBAGE VALUES.   SUCH IS THE ONLY USE OF      M15. 273
C)    THE VALUES GOTTEN BY CALLING THIS SUBROUTINE ---- FOR THE CASE-   M15. 274
C)    SUMMARY PRINTOUT.   HENCE IF ONE WANTS TO CONVERT TIME INTO       M15. 275
C)    DOLLARS, OR SOME OTHER MEASURE OF JOB EFFORT, IT IS EASILY DONE.  M15. 276
C     INCLUDE  '[SCOTT]COMMUK.FOR' --- SHARE WITH "SETTYM" IN-LINE:     M37. 865
      COMMON /TIMERS/ CPUTIME                                           M37. 866
      INTEGER*4 CPUTIME                                                 M37. 867
      COMMON /TIMER2/ L4CPU,CPUTIME_CODE,CPUTIME_ADR,ZERO,ZEROFIN       M37. 868
      INTEGER*2 L4CPU,CPUTIME_CODE                                      M37. 869
      INTEGER*4 CPUTIME_ADR,ZERO,ZEROFIN,SYS$GETJPI,NOW_CPUTIME         M37. 870
      DATA CPUTIME_CODE /1031/                                          M37. 871
      DATA L4CPU /4/                                                    M37. 872
      CPUTIME_ADR=%LOC(NOW_CPUTIME)                                     M24.  87
      IF (.NOT.SYS$GETJPI(,,,L4CPU,,,)) THEN                            M24.  88
                  WRITE(6,*) 'ERROR IN ANOTHER PRIVATE PLACE'           M24.  89
                  ENDIF                                                 M24.  90
      D1=(NOW_CPUTIME-CPUTIME)/100.0                                    M24.  91
      D2=0.0                                                            M24.  92
      RETURN                                                            M40.  82
      END                                                               M40.  83
      SUBROUTINE SETTYM                                                 M24. 105
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C       VAX-11  INSTALLATION-DEPENDENT EMTP MODULE.                     M37. 873
C     CALLED ONLY BY VAX "RUNTYM";  DESTROY FOR OTHER COMPUTERS.        M37. 874
C     INCLUDE  '[SCOTT]COMMUK.FOR' --- SHARE WITH "RUNTYM" IN-LINE:     M37. 875
      COMMON /TIMERS/ CPUTIME                                           M37. 876
      INTEGER*4 CPUTIME                                                 M37. 877
      COMMON /TIMER2/ L4CPU,CPUTIME_CODE,CPUTIME_ADR,ZERO,ZEROFIN       M37. 878
      INTEGER*2 L4CPU,CPUTIME_CODE                                      M37. 879
      INTEGER*4 CPUTIME_ADR,ZERO,ZEROFIN,SYS$GETJPI,NOW_CPUTIME         M37. 880
      DATA CPUTIME_CODE /1031/                                          M37. 881
      DATA L4CPU /4/                                                    M37. 882
      CPUTIME_ADR=%LOC(CPUTIME)                                         M24. 118
      IF (.NOT.SYS$GETJPI(,,,L4CPU,,,)) THEN                            M24. 119
                  WRITE(6,*) 'ERROR IN A PRIVATE PLACE'                 M24. 120
                  ENDIF                                                 M24. 121
      RETURN                                                            M24. 122
      END                                                               M24. 123
      SUBROUTINE TIME44 ( A )                                           M40.  84
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    THE PURPOSE OF SUBROUTINE  TIME44  IS TO INTERROGATE THE          M15. 277
C)    INSTALLATION CLOCK, AND RETURN THE WALL-CLOCK TIME THROUGH THE    M15. 278
C)    ARGUMENT OF THE SUBROUTINE.   EIGHT BCD CHARACTERS ARE ALLOWED,   M15. 279
C)    WITH THE FIRST (LEFT) FOUR CHARACTERS TO BE PLACED IN  A(1) ,     M15. 280
C)    AND THE FINAL (RIGHT) FOUR PLACED IN  A(2) .   A STATEMENT LIKE   M15. 281
C)             WRITE (LUNIT6, 4041)  A                                  M17.  97
C)        4041 FORMAT ( 1X, 2A4 )                                       M15. 283
C)    THUS OUTPUTS THE WALL-CLOCK TIME AS FIRST HOURS, THEN MINUTES,    M15. 284
C)    AND FINALLY SECONDS, SEPARATED BY PERIODS (HH.MM.SS) .            M15. 285
C)    SUBROUTINE  TIME44  IS OF COURSE INSTALLATION DEPENDENT.          M15. 286
C)    INSTALLATION-DEPENDENT  EMTP  MODULE WRITTEN FOR THE  DEC         M24.  93
C)    VAX-11/780.    'TIME'  IS A  DEC  SYSTEM SUBROUTINE WHICH         M24.  94
C)    RETURNS THE WALL-CLOCK TIME AS AN 8-BYTE CHARACTER STRING.        M24.  95
C)    THIS IS JUST WHAT THE EMTP NEEDS, EXCEPT THAT WE WANT PERIODS     M24.  96
C)    RATHER THAN COLONS, AND OF COURSE WE REQUIRE  2A4  FORMAT.        M24.  97
      REAL*8        A(2)                                                M24.  98
      LOGICAL*1   CHAR(8)                                               M24.  99
      CALL TIME ( CHAR(1) )                                             M24. 100
      ENCODE (4, 2741, A(1) )   CHAR(1), CHAR(2), CHAR(4)               M24. 101
 2741 FORMAT ( 2A1,  1H.,  A1 )                                         M24. 102
      ENCODE (4, 2754, A(2) )   CHAR(5), CHAR(7), CHAR(8)               M24. 103
 2754 FORMAT ( A1,  1H.,  2A1 )                                         M24. 104
      RETURN                                                            M40.  85
      END                                                               M40.  86
      SUBROUTINE CIMAGE                                                 M27.  33
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C       VAX-11  INSTALLATION-DEPENDENT EMTP MODULE WHICH SERVES         M36. 215
C     TO RETURN THE NEXT INPUT CARD.  ALL SYSTEMS WILL SUBSTITUTE.      M36. 216
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      DIMENSION  BUFF10(10)                                             M29.  82
      EQUIVALENCE ( BUFF10(1), ABUFF(1) )                               M29.  83
      BYTE  FILEN(25)                                                   M27.  35
      DIMENSION  TEXTAX(60), JPNTR(52), TEXTAY(50), AUPPER(10)          M29.  85
      EQUIVALENCE ( AUPPER(1), TEXCOL(1) )                              M27.  37
C     BURROUGHS: PRESERVE LOCAL VARIABLE BETWEEN MODULE CALLS:          M35. 244
      DATA  N8   /  0  /        ! REMEMBER LAST $-CARD NUMBER           M35. 245
      DATA  CHARC   / 1HC /                                             M27.  38
      DATA  TEXT4   / 1H9 /                                             M27.  39
      DATA  CHTACS   / 4HTACS /                                         M28. 131
      DATA  TEXT5   / 6HBLANK   /                                       M27.  40
C               *****    REQUEST NO. 1.    "$ATTACH"      *****  *****  M27.  41
      DATA  TEXTAY(1)   /  6HA       /                                  M27.  42
      DATA   JPNTR(1)   /    1       /                                  M27.  43
      DATA  TEXTAX(1)   /  6HATTACH  /                                  M27.  44
C               *****    REQUEST NO. 2.    "$PUNCH"       *****  *****  M27.  45
      DATA  TEXTAY(2)   /  6HP       /                                  M27.  46
      DATA   JPNTR(2)   /    2       /                                  M27.  47
      DATA  TEXTAX(2)   /  6HPUNCH   /                                  M27.  48
C               *****    REQUEST NO. 3.    "$OUTPUT"      *****  *****  M27.  49
      DATA  TEXTAY(3)   /  6HOUT     /                                  M29.  91
      DATA   JPNTR(3)   /    3       /                                  M27.  51
      DATA  TEXTAX(3)   /  6HOUTPUT  /                                  M27.  52
C               *****    REQUEST NO. 4.    "$SAVE"        *****  *****  M27.  53
      DATA  TEXTAY(4)   /  6HS       /                                  M27.  54
      DATA   JPNTR(4)   /    4       /                                  M27.  55
      DATA  TEXTAX(4)   /  6HSAVE    /                                  M27.  56
C               *****    REQUEST NO. 5.    "$SPYDATA"     *****  *****  M36. 217
C                   FORMER "$INCLUDE", WHICH IS NOW IN "EREXIT".        M36. 218
      DATA  TEXTAY(5)   /  6HSPY     /                                  M36. 219
      DATA   JPNTR(5)   /    5       /                                  M27.  59
      DATA  TEXTAX(5)   /  6HSPYDAT  /                                  M36. 220
      DATA  TEXTAX(6)   /  6HA       /                                  M36. 221
C               *****    REQUEST NO. 6.    "$DISABLE"     *****  *****  M27.  62
      DATA  TEXTAY(6)   /  6HD       /                                  M27.  63
      DATA   JPNTR(6)   /    7       /                                  M27.  64
      DATA  TEXTAX(7)   /  6HDISABL  /                                  M27.  65
      DATA  TEXTAX(8)   /  6HE       /                                  M27.  66
C               *****    REQUEST NO. 7.    "$ENABLE"      *****  *****  M27.  67
      DATA  TEXTAY(7)   /  6HE       /                                  M27.  68
      DATA   JPNTR(7)   /    9       /                                  M27.  69
      DATA  TEXTAX(9)   /  6HENABLE  /                                  M27.  70
C               *****    REQUEST NO. 8.    "$RETURN"      *****  *****  M27.  71
      DATA  TEXTAY(8)   /  6HR       /                                  M27.  72
      DATA   JPNTR(8)   /    10      /                                  M27.  73
      DATA  TEXTAX(10)  /  6HRETURN  /                                  M27.  74
C               *****    REQUEST NO. 9.    "NEWFILE"      *****  *****  M27.  75
      DATA  TEXTAY(9)   /  6HN       /                                  M27.  76
      DATA   JPNTR(9)   /    11      /                                  M27.  77
      DATA  TEXTAX(11)  /  6HNEWFIL  /                                  M27.  78
      DATA  TEXTAX(12)  /  6HE       /                                  M27.  79
C               *****    REQUEST NO. 10.   "NEW EPSILN"   *****  *****  M35. 246
      DATA  TEXTAY(10)  /  6HNE      /                                  M35. 247
      DATA  JPNTR(10)   /    13      /                                  M27.  82
      DATA  TEXTAX(13)  /  6HNEW     /                                  M35. 248
      DATA  TEXTAX(14)  /  6HEPSILN  /                                  M35. 249
C               *****    REQUEST NO. 11.   "DELETE"       *****  *****  M27.  85
      DATA  TEXTAY(11)  /  6HDE      /                                  M27.  86
      DATA  JPNTR(11)   /    15      /                                  M27.  87
      DATA  TEXTAX(15)  /  6HDELETE  /                                  M27.  88
C               *****    REQUEST NO. 12.   "MONITOR"      *****  *****  M27.  89
      DATA  TEXTAY(12)  /  6HM       /                                  M27.  90
      DATA  JPNTR(12)   /    16      /                                  M27.  91
      DATA  TEXTAX(16)  /  6HMONITO  /                                  M27.  92
      DATA  TEXTAX(17)  /  6HR       /                                  M27.  93
C               *****    REQUEST NO. 13.   "LISTOFF"      *****  *****  M27.  94
      DATA  TEXTAY(13)  /  6HLF      /                                  M27.  95
      DATA  JPNTR(13)   /    18      /                                  M27.  96
      DATA  TEXTAX(18)  /  6HLISTOF  /                                  M27.  97
      DATA  TEXTAX(19)  /  6HF       /                                  M27.  98
C               *****    REQUEST NO. 14.   "LISTON"       *****  *****  M27.  99
      DATA  TEXTAY(14)  /  6HLN      /                                  M27. 100
      DATA  JPNTR(14)   /    20      /                                  M27. 101
      DATA  TEXTAX(20)  /  6HLISTON  /                                  M27. 102
C               *****    REQUEST NO. 15.   "VINTAGE"      *****  *****  M28. 132
      DATA  TEXTAY(15)  /  6HV       /                                  M28. 133
      DATA  JPNTR(15)   /    21      /                                  M28. 134
      DATA  TEXTAX(21)  /  6HVINTAG  /                                  M28. 135
      DATA  TEXTAX(22)  /  6HE       /                                  M28. 136
C               *****    REQUEST NO. 16.   "OLDFILE"      *****  *****  M28. 137
      DATA  TEXTAY(16)  /  6HOLD     /                                  M28. 138
      DATA   JPNTR(16)  /    23      /                                  M28. 139
      DATA  TEXTAX(23)  /  6HOLDFIL  /                                  M28. 140
      DATA  TEXTAX(24)  /  6HE       /                                  M28. 141
C               *****    REQUEST NO. 17.   "STOP"         *****  *****  M28. 142
      DATA  TEXTAY(17)  /  6HST      /                                  M28. 143
      DATA   JPNTR(17)  /    25      /                                  M28. 144
      DATA  TEXTAX(25)  /  6HSTOP    /                                  M28. 145
C               *****    REQUEST NO. 18.   "WATCH5"       *****  *****  M28. 146
      DATA  TEXTAY(18)  /  6HW       /                                  M28. 147
      DATA  JPNTR(18)   /    26      /                                  M28. 148
      DATA  TEXTAX(26)  /  6HWATCH5  /                                  M28. 149
C               *****    REQUEST NO. 19.   "COMMENT"      *****  *****  M31. 112
      DATA  TEXTAY(19)  /  6HCOM     /                                  M31. 113
      DATA   JPNTR(19)  /    27      /                                  M31. 114
      DATA  TEXTAX(27)  /  6HCOMMEN  /                                  M31. 115
      DATA  TEXTAX(28)  /  6HT       /                                  M31. 116
C               *****    REQUEST NO. 20.   "WIDTH"        *****  *****  M32. 127
      DATA  TEXTAY(20)  /  6HWI      /                                  M32. 128
      DATA   JPNTR(20)  /    29      /                                  M32. 129
      DATA  TEXTAX(29)  /  6HWIDTH   /                                  M32. 130
C               *****    REQUEST NO. 21.   "UNITS"        *****  *****  VAX.   4
      DATA  TEXTAY(21)  /  6HU       /                                  VAX.   5
      DATA  JPNTR(21)   /    30      /                                  M42. 260
      DATA  TEXTAX(30)  /  6HUNITS   /                                  VAX.   6
C               *****   BOUNDS FOLLOW ...                               VAX.   7
      DATA   JPNTR(22)  /    31      /                                  VAX.   8
      DATA  JPNTR(23)   /     0      /                                  VAX.   9
      DATA  N11    /   0    /                                           M31. 119
      DATA  N12   /  99999  /                                           M28. 152
      DATA  N13   /  99999  /                                           M28. 153
      IF ( IPRSUP  .GE.  10 )                                           M28. 154
     1 WRITE (LUNIT6, 987)  LUNIT5, LUNIT6, NOUTPR, NUMDCD              M36. 222
  987 FORMAT ( ' BEGIN CIMAGE.  LUNIT5, LUNIT6, NOUTPR,',               M36. 223
     1         ' NUMDCD =',  4I5  )                                     M36. 224
 1000 IF ( M4PLOT .EQ. 1 )  CALL EMTSPY  ! INTERACTIVE USAGE            M36. 225
      IF ( LUNIT5  .GT.  0 )                                            M36. 226
     1 READ (LUNIT5, 3000, END=4000)   BUFF10                           M36. 227
 3000 FORMAT( 10A8 )                                                    M36. 228
      IF ( LUNIT5  .LE.  0 )   CALL NEXTCARD                            M36. 229
      IF ( KILL .GT. 0 )  GO TO 4000    ! "NEXTCARD" EOF JUMP           M37. 883
      IF ( LUNSAV(5)  .NE.  -5 )                                        M36. 230
     1 NUMDCD = NUMDCD + 1                                              M36. 231
      DECODE (2, 3012, ABUFF)  TEXT1, TEXT2                             M31. 120
 3012 FORMAT ( 2A1 )                                                    M31. 121
      IF ( TEXT1  .NE.  CHARC )   GO TO 3034                            M31. 122
      IF ( TEXT2 .NE. BLANK )     GO TO 3034                            M31. 123
 1036 IF ( NOUTPR  .NE.  0 )   GO TO 1000                               M31. 124
      IF ( N11     .NE.  0 )   GO TO 1000                               M31. 125
      IF ( KOL132  .EQ.  132 )                                          M32. 133
     1 WRITE (LUNIT6, 3015)  BUFF10                                     M32. 134
 3015 FORMAT ( 14H COMMENT CARD.,  37X,  1H1,  10A8  )                  M31. 127
      IF ( KOL132  .NE.  132 )                                          M32. 135
     1 WRITE (LUNIT6, 3016)  ( ABUFF(J), J=1, 4 )                       M32. 136
 3016 FORMAT ( 14H COMMENT CARD.,  37X,  1H1,  3A8,  A5  )              M32. 137
      GO TO 1000                                                        M31. 128
 3034 IF ( NOUTPR  .NE.  0 )   GO TO 3035                               M32. 138
      IF ( KOL132  .EQ.  132 )                                          M32. 139
     1 WRITE (LUNIT6, 3006)    BUFF10                                   M29. 210
 3006 FORMAT( 51X, 1H1, 10A8 )                                          M27. 114
      IF ( KOL132  .NE.  132 )                                          M32. 140
     1 WRITE (LUNIT6, 3007)   ( ABUFF(J), J=1, 4 )                      M32. 141
 3007 FORMAT ( 51X,  1H1,  3A8,  A5  )                                  M32. 142
 3035 IF ( N13  .GT.  0 )   GO TO 3011                                  M32. 143
      PRINT 3009, NUMDCD, (ABUFF(I), I=1, 9)                            M28. 156
 3009 FORMAT ( 1X, I5,  2H :,  9A8 )                                    M28. 157
      N13 = N12                                                         M28. 158
 3011 N13 = N13 - 1                                                     M28. 159
      DECODE (6, 3037, ABUFF(1) ) TEXT2                                 M36. 232
 3037 FORMAT (A6)                                                       M27. 123
      IF ( TEXT2 .NE. TEXT5 )   GO TO 3040                              M36. 233
      IF ( N8  .EQ.  6 )   GO TO 3044                                   M32. 144
 3039 DO 3038 I=1, 10                                                   M29. 213
 3038 ABUFF (I) = BLANK                                                 M27. 126
      GO TO 3233                                                        M27. 127
 3040 IF ( CHCONT  .EQ.  TEXT4 )  GO TO 3233                            M36. 234
      DECODE (80, 3041, ABUFF(1) )   TEXCOL                             M27. 129
 3041 FORMAT ( 80A1 )                                                   M27. 130
C     DAN GOLDSWORTHY HAD TROUBLE WITH $LISTOFF WITHIN $INCLUDE         M36. 235
C     WHICH WAS WITHIN TACS SUPPLEMENTAL VARIABLES.  WSM+THL            M36. 236
      IF ( ABUFF(1) .NE. 8H$LISTOFF   .AND.                             M36. 237
     1     ABUFF(1) .NE. 8H$LISTON  )   GO TO 3042                      M36. 238
      GO TO 3246                                                        M36. 239
C     CHCONT IS 'TACS' IF CIMAGE CALLED FROM WITHIN TACS FORTRAN EXPRESSM28. 162
 3042 IF ( CHCONT .EQ. CHTACS )   GO TO 3233                            M36. 240
      IF (TEXCOL(1) .EQ. CHCONT) GO TO 3246                             M36. 241
      IF ( N8  .NE.  6 )   GO TO 1144                                   M36. 242
 3044 IF ( NOUTPR  .EQ.  0 )                                            M32. 146
     1 WRITE (LUNIT6, 3045)                                             M32. 147
 3045 FORMAT (  25H+COMMENT CARD (IMPLICIT).    )                       M32. 148
      GO TO 1000                                                        M32. 149
 1144 DO 3231  K=1, 80                                                  M37. 884
      IF ( TEXCOL(K)  .EQ.  CSEPAR )   GO TO 3237                       M27. 134
      IF ( TEXCOL(K)  .EQ.  CHCONT )  GO TO 3237                        M27. 135
 3231 CONTINUE                                                          M27. 136
 3233 KOLBEG = -INTINF                                                  M27. 137
      GO TO 7014                                                        M27. 138
 3237 KOLBEG = 1                                                        M27. 139
      GO TO 7014                                                        M27. 140
 3246 KOLBEG =  2                                                       M27. 141
 3251 NRIGHT = -2                                                       M29. 227
      CALL FREONE ( D1 )                                                M35. 250
      IF ( IPRSUP  .GE.  1 )                                            M27. 144
     1 WRITE (LUNIT6, 3281)  NFRFLD,     TEXTA6(1), TEXTA6(2)           M28. 164
 3281 FORMAT (  /,  ' NFRFLD =',  I8,  5X,                              M28. 165
     1  'TEXTA6 =',  2A7  )                                             M27. 147
      NRIGHT = 0                                                        M31. 131
      DO 3307  I=1, 200                                                 M27. 149
      N1 = JPNTR(I)                                                     M27. 150
      N2 = JPNTR(I+1) - 1                                               M27. 151
      IF ( N2  .LT.  0 )   GO TO 3319                                   M27. 152
      IF ( IPRSUP  .GE.  35 )                                           M27. 153
     1 WRITE (LUNIT6, 3285)  I,  ( TEXTAX(J), J=N1, N2 )                M27. 154
 3285 FORMAT (     21H SPECIAL-REQUEST WORD,  I4,  2H .,  10A6  )       M27. 155
      IF ( TEXTAX(N1)  .EQ.  BLANK )   GO TO 3306                       M27. 156
      L = 0                                                             M27. 157
      N3 = N2 - N1 + 1                                                  M27. 158
      IF ( N3  .NE.  NFRFLD )   GO TO 3306                              M27. 160
      DO 3291  J=N1, N2                                                 M34.   1
      L = L + 1                                                         M27. 162
      IF ( TEXTA6(L)  .NE.  TEXTAX(J) )   GO TO 3306                    M27. 163
 3291 CONTINUE                                                          M27. 164
      IF ( IPRSUP  .GE.  2 )                                            M27. 165
     1 WRITE (LUNIT6, 3292)  I, N8, TEXTA6(1), TEXTAY(I)                M27. 166
 3292 FORMAT ( /,  ' KEY-WORD FOUND.  I, N8 =',  2I5,                   M27. 167
     1         5X,  'TEXTA6, TEXTAY =',  2A7  )                         M27. 168
 3294 IF ( N8  .NE.  6 )   GO TO 3301                                   M27. 169
      IF ( I  .NE.  7 )    GO TO 1036                                   M27. 170
 3301 N8 = I                                                            M31. 132
      GO TO ( 4100, 4200, 4300, 4400, 4500, 4600, 4700, 4800, 4900,     M27. 172
     1  5000, 5100, 5200, 5300, 5400, 5500, 5600, 5700, 5800,           M35. 251
     2  5900, 6000, 6100),  N8                                          M32. 150
 3306 IF ( TEXTA6(1)  .EQ.  TEXTAY(I) )   GO TO 3294                    M27. 174
 3307 CONTINUE                                                          M27. 175
 3319 WRITE (LUNIT6, 3230)                                              M27. 176
 3230 FORMAT ( ' ILLEGAL $-CARD.   STOP AT S.N. 3319 OF                 M27. 177
     1 "CIMAGE" .')                                                     M27. 178
      CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD          M37. 885
C               *****    REQUEST NO. 1.    "$ATTACH"      *****  *****  M27. 180
 4100 TEXT1 = 6HATTACH                                                  M27. 181
      GO TO 4506                                                        M27. 182
C               *****    REQUEST NO. 2.    "$PUNCH"       *****  *****  M27. 183
 4200 TEXT1 = TEXTAX(2)                                                 M27. 184
 4206 N2 = LUNIT7                                                       M27. 185
 4209 NFRFLD = 1                                                        M27. 186
      CALL FREONE ( D11 )                                               M35. 252
      N1 = D11                                                          M27. 188
 4225 IF ( N1  .LE.  0 )   N1 = N2                                      M27. 189
      IF ( N8  .EQ.  8 )   GO TO 4817                                   M27. 190
      IF ( NOUTPR  .EQ.  0 )                                            M27. 191
     1 WRITE (LUNIT6, 4231)  N1, TEXT1                                  M27. 192
 4231 FORMAT ( 10H+COPY FILE,  I4,  6H   TO ,  A6,  2H .   )            M27. 193
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29. 243
      REWIND N1                                                         M27. 194
      DO 4246  K=1, INTINF                                              M27. 195
      READ (N1, 3000, END=4249)  AUPPER                                 M27. 196
C     BECAUSE DEC FORTRAN DOES NOT HONOR THE CDC  "PUNCH"  STATEMENT    M27. 197
C     (TO DIRECTLY PUNCH CARDS), I WRITE TO UNIT 7 AS NEXT BEST         M27. 198
C     THING.  WSM.  JAN, 1980.                                          M27. 199
      IF ( N8  .EQ.  2 )                                                M27. 200
     1 WRITE (LUNIT7, 3000)  AUPPER                                     M27. 201
      IF ( N8  .EQ.  3 )                                                M27. 202
     1 WRITE (LUNIT6, 4238) K, AUPPER                                   M27. 203
 4238 FORMAT ( 20X,  6HRECORD,  I5,  5H .  1,  10A8 )                   M27. 204
 4246 CONTINUE                                                          M27. 205
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29. 244
 4249 REWIND N1                                                         M27. 206
      GO TO 1000                                                        M27. 207
C               *****    REQUEST NO. 3.    "$OUTPUT"      *****  *****  M27. 208
 4300 TEXT1 = TEXTAX(3)                                                 M27. 209
      GO TO 4206                                                        M27. 210
C               *****    REQUEST NO. 4.    "$SAVE"        *****  *****  M27. 211
 4400 TEXT1 = 6HSAVED                                                   M27. 212
      GO TO 4506                                                        M27. 213
 4423 CLOSE ( UNIT=N7, DISPOSE='DELETE' )                               M27. 214
      OPEN  ( UNIT=N7, TYPE='NEW',  FORM='FORMATTED',  NAME=FILEN )     M27. 215
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29. 245
      REWIND N6                                                         M27. 216
      DO 4429  K=1, INTINF                                              M27. 217
      READ (N6, 3000, END=4436)  AUPPER                                 M27. 218
 4429 WRITE (N7, 3000 )   AUPPER                                        M27. 219
 4436 CLOSE ( UNIT=N7,  DISPOSE='SAVE' )                                M27. 220
C     SEGMENTED, 1, VAX E/T CAN SKIP TRANSLATION OF REWIND:             M29. 246
      REWIND N6                                                         M27. 221
      GO TO 1000                                                        M27. 222
C               *****    REQUEST NO. 5.    "$SPY"         *****  *****  M36. 245
 4500 TEXT1 = 6HSPYING                                                  M36. 246
      MUNTSV(2) = 2288  ! REMEMBER $SPY FOR EOF TIME IN "SPYING"        M36. 247
      MUNTSV(1) = MUNIT5   ! REMEMBER SPY INPUT CHANNEL TILL NOW        M36. 248
      MUNIT5 = 17   ! SPECIAL I/O UNIT USED FOR $SPY CONNECTION         M36. 249
      KFILE5 = 1  ! FLAG SO "SPYING" KNOWS MUNIT5 OPENED TO DISK        M36. 250
 4506 N4 = 0                                                            M27. 225
      ENCODE (25, 4523, FILEN(1))                                       M27. 226
 4523 FORMAT ( 25X )                                                    M27. 227
      DO 4532  K=KOLBEG, 80                                             M27. 228
      IF ( TEXCOL(K)  .EQ.  BLANK )   GO TO 4532                        M27. 229
      IF ( TEXCOL(K)  .EQ.  CSEPAR )  GO TO 4536                        M27. 230
      IF ( TEXCOL(K)  .EQ.   1H(  )   GO TO 4536                        M29. 247
      N4 = N4 + 1                                                       M27. 231
      ENCODE (1, 3041, FILEN(N4))  TEXCOL(K)                            M27. 232
 4532 CONTINUE                                                          M27. 233
      K = 80                                                            M29. 248
 4536 KOLBEG = K + 1                                                    M29. 249
      NFRFLD = 1                                                        M31. 135
      CALL FREONE ( D11 )                                               M35. 253
      N7 = D11                                                          M27. 237
      IF ( N8  .NE.  4 )   GO TO 4557                                   M31. 136
      CALL FREONE ( D11 )                                               M35. 254
      N6 = D11                                                          M27. 240
 4557 IF ( N6  .EQ.  0 )   N6 = LUNIT7                                  M27. 241
      IF ( N8 .EQ. 5 )  N7 = MUNIT5   ! $SPY USES THIS CHANNEL          M36. 251
      IF ( N7  .GT.  0 )   GO TO 4570                                   M27. 242
      DO 4562  K=1, 15                                                  M27. 243
      IF ( LUNSAV(K)  .LE.  0 )   GO TO 4568                            M27. 244
 4562 CONTINUE                                                          M27. 245
      WRITE (LUNIT6, 4565)                                              M27. 246
 4565 FORMAT ( /,  10( 7H ERROR, )  )                                   M32. 151
      WRITE (LUNIT6, 4566)  LUNSAV                                      M27. 248
 4566 FORMAT ( 5X,  ' ALL I/O CHANNELS OCCUPIED.  KILL RUN',            M27. 249
     1              ' AT  S.N. 4566  OF  "CIMAGE" .'  ,/,  20I5  )      M27. 250
      CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD          M37. 886
 4568 N7 = K                                                            M27. 252
 4570 IF ( NOUTPR  .EQ.  0 )                                            M27. 253
     1 WRITE (LUNIT6, 4572)  TEXT1, FILEN,  N7                          M27. 254
 4572 FORMAT ( 1H+,  A6,  6H FILE:,  25A1,  7H UNIT =,  I3  )           M27. 255
      IF ( N8  .EQ.  4 )   GO TO 4423                                   M27. 256
      IF ( N8  .EQ.  9 )   GO TO 4907                                   M27. 257
      IF ( N8  .EQ.  11 )  GO TO 5106                                   M27. 258
      IF ( N8  .EQ.  16 )  GO TO 5608                                   M28. 168
      CLOSE (UNIT=N7)                                                   M27. 259
      OPEN (UNIT=N7, STATUS='OLD', FORM='FORMATTED', FILE=FILEN )       M36. 252
      REWIND N7                                                         M27. 261
      IF ( N8 .EQ. 5 ) CALL SPYING   ! PROCESS $SPY COMMAND FILE        M36. 253
      GO TO 1000                                                        M27. 265
C               *****    REQUEST NO. 6.    "$DISABLE"     *****  *****  M27. 266
 4600 IF ( NOUTPR  .EQ.  0 )                                            M27. 267
     1 WRITE (LUNIT6, 4612)                                             M27. 268
 4612 FORMAT (  26H+BEGIN DATA TO BE IGNORED.  )                        M27. 269
      GO TO 1000                                                        M27. 270
C               *****    REQUEST NO. 7.    "$ENABLE"      *****  *****  M27. 271
 4700 IF ( NOUTPR  .EQ.  0 )                                            M27. 272
     1 WRITE (LUNIT6, 4714)                                             M27. 273
 4714 FORMAT (  27H+END OF DATA TO BE IGNORED.  )                       M27. 274
      GO TO 1000                                                        M27. 275
C               *****    REQUEST NO. 8.    "$RETURN"      *****  *****  M27. 276
 4800 N2 = LUNIT4                                                       M27. 277
      GO TO 4209                                                        M27. 278
 4817 CLOSE (UNIT=N1)                                                   M27. 279
      IF ( NOUTPR  .EQ.  0 )                                            M27. 280
     1 WRITE (LUNIT6, 4823)  N1                                         M27. 281
 4823 FORMAT (  19H+CLOSE FILE ON UNIT,  I3,  2H .  )                   M27. 282
      IF ( N1  .NE.  LUNIT5 )   GO TO 1000                              M29. 277
      NOUTPR = 1                                                        M29. 278
      GO TO 1000                                                        M31. 137
C               *****    REQUEST NO. 9.    "$NEWFILE"     *****  *****  M27. 284
 4900 TEXT1 = 6HNEWFIL                                                  M27. 285
      N2 = LUNIT4                                                       M27. 286
      GO TO 4506                                                        M27. 287
 4907 OPEN (UNIT=N7, TYPE='NEW', FORM='UNFORMATTED', NAME=FILEN )       M28. 169
      GO TO 1000                                                        M27. 289
C               *****    REQUEST NO. 10.   "NEW EPSILN"   *****  *****  M35. 255
 5000 NFRFLD = 1                                                        M35. 256
      D1 = EPSILN                                                       M35. 257
      CALL FREONE ( EPSILN )                                            M35. 258
      IF ( NOUTPR  .EQ.  0 )                                            M35. 259
     1 WRITE (LUNIT6, 5017)  D1, EPSILN                                 M35. 260
 5017 FORMAT ( 28H+ EPSILN CHANGE.  OLD, NEW =,  2E11.2 )               M35. 261
      GO TO 1000                                                        M35. 262
C               *****    REQUEST NO. 11.   "DELETE"       *****  *****  M27. 298
 5100 TEXT1 = 6HDELETE                                                  M27. 299
      GO TO 4506                                                        M27. 300
 5106 OPEN ( UNIT=N7, TYPE='OLD', NAME=FILEN )                          M27. 301
      CLOSE( UNIT=N7, DISPOSE='DELETE' )                                M27. 302
      GO TO 1000                                                        M27. 303
C               *****    REQUEST NO. 12.   "MONITOR"      *****  *****  M27. 304
 5200 IF ( NOUTPR  .NE.  0 )   GO TO 5219                               M31. 138
      PRINT 3006,  BUFF10                                               M29. 295
      PRINT 5214, NUMDCD                                                M27. 307
 5214 FORMAT (  '+CRT MONITOR.  CARD NUMBER =',  I5  )                  M27. 308
 5219 GO TO 1000                                                        M27. 309
C               *****    REQUEST NO. 13.   "LISTOFF"      *****  *****  M27. 310
 5300 IF ( NOUTPR  .NE.  0 )   GO TO 5324                               M27. 311
      WRITE (LUNIT6, 5307)  NUMDCD                                      M27. 312
 5307 FORMAT ( '+TURN OFF INPUT LISTING AT CARD',  I5 )                 M27. 313
      NOUTPR = 1                                                        M27. 314
 5324 GO TO 1000                                                        M27. 315
C               *****    REQUEST NO. 14.   "LISTON"       *****  *****  M27. 316
 5400 WRITE (LUNIT6, 5404)                                              M27. 317
 5404 FORMAT ( 51X,   '1$LISTON'  )                                     M27. 318
      NOUTPR = 0                                                        M27. 319
      WRITE (LUNIT6, 5412)  NUMDCD                                      M27. 320
 5412 FORMAT (  '+TURN ON INPUT LISTING AT CARD',  I5  )                M27. 321
      GO TO 1000                                                        M27. 322
C               *****    REQUEST NO. 15.   "VINTAGE"      *****  *****  M28. 170
 5500 NFRFLD = 1                                                        M28. 171
      CALL FREONE ( D11 )                                               M35. 263
      MOLDAT = D11                                                      M28. 173
      IF ( NOUTPR  .EQ.  0 )                                            M28. 174
     1 WRITE (LUNIT6, 5518)  MOLDAT                                     M28. 175
 5518 FORMAT (  13H+NEW MOLDAT =,  I4,  5X,  '(DATA VINTAGE)'  )        M28. 176
      GO TO 1000                                                        M28. 177
C               *****    REQUEST NO. 16.   "OLDFILE"      *****  *****  M28. 178
 5600 TEXT1 = 6HOLDFIL                                                  M28. 179
      N2 = LUNIT2                                                       M28. 180
      GO TO 4506                                                        M28. 181
 5608 CLOSE (UNIT=N7)                                                   M28. 182
      OPEN (UNIT=N7, TYPE='OLD', FORM='UNFORMATTED', NAME=FILEN )       M28. 183
      GO TO 1000                                                        M28. 184
C               *****    REQUEST NO. 17.   "STOP"         *****  *****  M28. 185
 5700 WRITE (LUNIT6, 5706)                                              M28. 186
 5706 FORMAT (  41H+STOP EXECUTION IMMEDIATELY, IN "CIMAGE". )          M28. 187
      CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD          M37. 887
C               *****    REQUEST NO. 18.   "WATCH5"       *****  *****  M28. 189
 5800 NFRFLD = 1                                                        M28. 190
      CALL FREONE ( D11 )                                               M35. 264
      N12 = D11                                                         M28. 192
      N13 = N12                                                         M28. 193
      IF ( NOUTPR  .EQ.  0 )                                            M28. 194
     1 WRITE (LUNIT6, 5812)  N12                                        M35. 265
 5812 FORMAT (  28H+PAINT INPUT DATA ON SCREEN.,  I8  )                 M28. 196
      GO TO 1000                                                        M28. 197
C               *****    REQUEST NO. 19.   "COMMENT"      *****  *****  M31. 139
 5900 N11 = N11 + 1                                                     M31. 140
      IF ( N11  .GE.  2 )  N11 = 0                                      M31. 141
      IF ( NOUTPR  .EQ.  0 )                                            M31. 142
     1 WRITE (LUNIT6, 5917)  N11                                        M31. 143
 5917 FORMAT (  38H+TOGGLE COMMENT CARD DESTRUCTION FLAG.,  I8  )       M31. 144
      GO TO 1000                                                        M31. 145
 6000 CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD          M42. 531
C               *****    REQUEST NO. 21.   "UNITS"        *****  *****  VAX.  10
 6100 NFRFLD = 1                                                        VAX.  11
      CALL FREFLD ( XOPT )                                              VAX.  12
      CALL FREFLD ( COPT )                                              VAX.  13
      IF ( NOUTPR  .EQ.  0 )                                            VAX.  14
     1 WRITE (LUNIT6, 6114)  XOPT, COPT                                 VAX.  15
 6114 FORMAT (  18H+NEW  XOPT, COPT =,  2E14.4  )                       VAX.  16
      XUNITS = 1000.                                                    VAX.  17
      IF( XOPT .GT. 0.0 )   XUNITS = TWOPI * XOPT                       VAX.  18
      GO TO 1000                                                        VAX.  19
C     ADDITIONAL KEY-WORD CODE GOES BELOW.                              VAX.  20
 4000 WRITE (LUNIT6, 4006)                                              M42. 532
 4006 FORMAT ( /,  1X, 85( 1H= ) ,/,                                    M37. 889
     1   40H END OF FILE ENCOUNTED IN "CIMAGE" WHILE,                   M37. 890
     2   46H ATTEMPTING TO READ ANOTHER DATA CARD.   STOP.              M37. 891
     3        ,/,  1X, 85( 1H= )     )                                  M37. 892
      CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD          M37. 893
C     UNIQUE EXIT OF MODULE, POSSIBLY AFTER ECHOING CARD IMAGE:         M29. 506
 7014 IF ( INECHO  .EQ.  0 )   RETURN                                   M27. 345
      ENTRY CECHO                                                       M27. 346
C     "STATISTICS" OVER12 ECHOS LUNIT5 CARD IMAGES OF BASE CASE         M32. 162
C     (READ IN OVER13, OVER15, MAYBE SUBTS3, SUBR31) SO EACH            M32. 163
C     ENERGIZATION CAN RE-CIMAGE THIS LUNT12 DATA. 2ND AND LATER        M32. 164
C     ONE SKIPS OVERLAY 12-15 CARDS WITH LUNIT5 READ IN OVER12          M32. 165
      IF ( NCHAIN  .LE.  15 )                                           M32. 166
     1 IPNTV(11) = IPNTV(11) + 1                                        M32. 167
      WRITE (INECHO, 3000)  BUFF10                                      M29. 507
      RETURN                                                            M27. 348
      ENTRY IBRINC                                                              
      IBR = IBR + 1                                                             
      XOPTBR(IBR) = XOPT                                                        
      COPTBR(IBR) = COPT                                                        
      RETURN                                                                    
      END                                                               M27. 349
      SUBROUTINE IOERR ( NADDR )                                        M42. 546
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      RETURN                                                            M42. 547
      END                                                               M42. 548
      SUBROUTINE CATERR ( NADDR, KODERR )                               M42. 549
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      RETURN                                                            M42. 550
      END                                                               M42. 551
      FUNCTION LOCF ( ARRAY )                                           M29. 921
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION  ARRAY(1)                                               M29. 922
      LOCF = LOCINT( ARRAY(1) ) / 2                                     M29. 923
      RETURN                                                            M42. 552
      END                                                               M42. 553
      FUNCTION  LOCINT ( IARRAY )                                       M29. 924
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION  IARRAY(1)                                              M29. 925
C     INSTALLATION-DEPENDENT EMTP MODULE.   THIS IS  VAX  VERSION.      M29. 926
C     FUNCTION  'LOCINT'  IS DESIGNED TO RETURN THE ADDRESS IN MEMORY   M24. 168
C     OF THE ARGUMENT, AS AN  INTEGER*4  WORD ADDRESS.   AN ARBITRARY   M24. 169
C     CONSTANT OFFSET IS ALLOWED, SINCE ONLY DIFFERENCES WILL EVER BE   M24. 170
C     USED BY THE EMTP.   NOTE VECTOR ARGUMENT  "IARRAY"  (WHICH        M29. 927
C     IS AN ASSUMPTION FOR ALL EMTP USAGE).                             M29. 928
      LOCINT = ( %LOC( IARRAY(1) ) )/4                                  M29. 929
      RETURN                                                            M05. 315
      END                                                               M05. 316
      FUNCTION RFUNL1 ( X )                                             M19. 208
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS FUNCTION PROVIDES FOR ALL REAL LIBRARY FUNCTIONS OF          M19. 209
C     A SINGLE REAL ARGUMENT.   ALL TRANSLATIONS WILL MAKE A            M19. 210
C     SUBSTITUTION.                                                     M19. 211
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      RFUNL1 = X                                                        M24. 176
      RETURN                                                            M24. 177
      ENTRY ABSZ   (X)                                                  M24. 178
      ABSZ = DABS(X)                                                    M24. 179
      RETURN                                                            M24. 180
      ENTRY ACOSZ  (X)                                                  M24. 181
      ACOSZ = DACOS ( X )                                               M35. 268
      RETURN                                                            M24. 186
      ENTRY AINTZ  (X)                                                  M24. 187
      AINTZ = DINT(X)                                                   M24. 188
      RETURN                                                            M24. 189
      ENTRY ALOGZ  (X)                                                  M24. 190
      ALOGZ = DLOG(X)                                                   M24. 191
      RETURN                                                            M24. 192
      ENTRY ALOG1Z (X)                                                  M24. 193
      ALOG1Z = DLOG10(X)                                                M24. 194
      RETURN                                                            M24. 195
      ENTRY ASINZ  (X)                                                  M24. 196
      ASINZ = DASIN(X)                                                  M24. 197
      RETURN                                                            M24. 198
      ENTRY ATANZ  (X)                                                  M24. 199
      ATANZ = DATAN(X)                                                  M24. 200
      RETURN                                                            M24. 201
      ENTRY COSZ   (X)                                                  M24. 202
      COSZ = DCOS(X)                                                    M24. 203
      RETURN                                                            M24. 204
      ENTRY COSHZ  (X)                                                  M24. 205
      COSHZ = DCOSH(X)                                                  M24. 206
      RETURN                                                            M24. 207
      ENTRY COTANZ (X)                                                  M24. 208
      Y = DSIN(X)                                                       M24. 209
      IF ( DABS(Y)*FLTINF  .GT.  1.0 )   GO TO 4783                     M24. 210
      WRITE (LUNIT6, 4761)  X                                           M24. 211
 4761 FORMAT ( /,  60H STOP.   TOO SMALL ARGUMENT AT  'COTANZ'  WITHIN  M24. 212
     1'RFUNL1' .,  E15.5  )                                             M24. 213
      CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD          M37. 894
 4783 COTANZ = DCOS(X) / Y                                              M24. 215
      RETURN                                                            M24. 216
      ENTRY EXPZ   (X)                                                  M24. 217
      IF ( X .GE. -87 ) GO TO 1488                                              
      EXPZ = 0.0                                                                
      RETURN                                                                    
 1488 EXPZ = DEXP(X)                                                    M24. 218
      RETURN                                                            M24. 219
      ENTRY SINZ   (X)                                                  M24. 224
      SINZ = DSIN(X)                                                    M24. 225
      RETURN                                                            M24. 226
      ENTRY SINHZ  (X)                                                  M24. 227
      SINHZ = DSINH(X)                                                  M24. 228
      RETURN                                                            M24. 229
      ENTRY SQRTZ  (X)                                                  M24. 230
      SQRTZ = DSQRT(X)                                                  M24. 231
      RETURN                                                            M24. 232
      ENTRY TANZ   (X)                                                  M24. 233
      TANZ = DTAN(X)                                                    M24. 234
      RETURN                                                            M24. 235
      ENTRY TANHZ  (X)                                                  M24. 236
      TANHZ = DTANH(X)                                                  M24. 237
      RETURN                                                            M24. 238
      END                                                               M19. 212
      SUBROUTINE TRGWND ( X, D17 )                                      M42. 571
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      D17 = X                                                           M42. 573
      IF ( DABS(X) .LT. 25000. )  GO TO 9000                                    
      N13 = X / TWOPI                                                           
      D17 = D17 - N13 * TWOPI                                                   
      IF ( IPRSUP .GE. 1 )                                              M42. 577
     1 WRITE (*, 3456)  NCHAIN, X, D17                                  M42. 578
 3456 FORMAT ( ' ANGLE UNWIND IN "TRGWND" CALLED BY "RFUNL1".',         M42. 579
     1         '   NCHAIN, X, D17 =',  I5, 2E25.16  )                   M42. 580
 9000 RETURN                                                            M42. 581
      END                                                               M42. 582
      FUNCTION  IFUNL1 ( D1 )                                           M28. 202
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS FUNCTION IS TO PROVIDE NEUTRAL NAMES ENDING IN "Z"           M28. 203
C     FOR ALL INTEGER LIBRARY FUNCTIONS OF ONE REAL ARGUMENT.           M28. 204
      IFUNL1 = D1                                                       M28. 205
      RETURN                                                            M28. 206
      ENTRY     INTZ ( D1 )                                             M28. 207
      INTZ = DINT ( D1 )                                                M28. 208
      RETURN                                                            M28. 209
      END                                                               M28. 210
      FUNCTION CFUNL1 ( X )                                             M19. 213
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      COMPLEX*16    CFUNL1, X, CEXPZ, CSQRTZ, CLOGZ                     M24. 239
      CFUNL1 = X                                                        M24. 240
      RETURN                                                            M24. 241
      ENTRY CEXPZ  (X)                                                  M24. 242
      CEXPZ = CDEXP(X)                                                  M31. 147
      RETURN                                                            M24. 244
      ENTRY CSQRTZ (X)                                                  M24. 245
      CSQRTZ = CDSQRT(X)                                                M31. 148
      RETURN                                                            M24. 247
      ENTRY CLOGZ (X)                                                   M24. 248
      CLOGZ = CDLOG(X)                                                  M31. 149
      RETURN                                                            M24. 250
      END                                                               M19. 217
      FUNCTION RFUNL2 ( X, Y )                                          M19. 218
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS FUNCTION PROVIDES FOR ALL REAL LIBRARY FUNCTIONS OF          M19. 219
C     TWO REAL ARGUMENTS.  ALL TRANSLATIONS WILL MAKE A                 M19. 220
C     SUBSTITUTION.                                                     M19. 221
      RFUNL2 = X                                                        M24. 251
      RETURN                                                            M24. 252
      ENTRY ATAN2Z (X,Y)                                                M24. 253
      ATAN2Z = 0.0                                                      M26.   2
      IF (X .NE. 0.0  .OR. Y .NE. 0.0) ATAN2Z = DATAN2(X,Y)             M26.   3
      RETURN                                                            M24. 255
      ENTRY SIGNZ  (X,Y)                                                M24. 256
      SIGNZ = DSIGN(X,Y)                                                M24. 257
      RETURN                                                            M24. 258
      ENTRY AMODZ(X,Y)                                                  M25.  69
      AMODZ = DMOD (X,Y)                                                M25.  70
      RETURN                                                            M25.  71
      ENTRY AMIN1Z(X,Y)                                                 M25.  72
      AMIN1Z = DMIN1(X,Y)                                               M25.  73
      RETURN                                                            M25.  74
      ENTRY AMAX1Z(X,Y)                                                 M25.  75
      AMAX1Z = DMAX1(X,Y)                                               M25.  76
      RETURN                                                            M35. 269
      END                                                               M19. 222
      FUNCTION CMPLXZ ( X, Y )                                          M19. 223
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS FUNCTION PROVIDES FOR ALL COMPLEX LIBRARY FUNCTIONS OF       M19. 224
C     TWO REAL ARGUMENTS.  ALL TRANSLATIONS WILL MAKE A                 M19. 225
C     SUBSTITUTION.                                                     M19. 226
C       VAX MODULE SWITCHED TO COMPLEX*16 (FROM *8) IN AUG 1981         M31. 150
      COMPLEX*16    CMPLXZ                                              M24. 259
      CMPLXZ = DCMPLX ( X, Y )                                          M31. 151
      RETURN                                                            M24. 265
      END                                                               M19. 227
      FUNCTION RFUNL3 ( X )                                             M19. 228
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS FUNCTION PROVIDES FOR ALL REAL LIBRARY FUNCTIONS OF          M19. 229
C     A SINGLE COMPLEX ARGUMENT.   ALL TRANSLATIONS WILL MAKE A         M19. 230
C     SUBSTITUTION.                                                     M19. 231
C       THIS VAX MODULE BECAME COMPLEX*16 (FROM *8) IN AUG 1981         M31. 152
      COMPLEX*16     X                                                  M24. 266
      RFUNL3 = 0.0                                                      M24. 269
      RETURN                                                            M24. 270
      ENTRY AIMAGZ (X)                                                  M24. 271
      AIMAGZ = DIMAG ( X )                                              M31. 153
      RETURN                                                            M24. 274
      ENTRY REALZ  (X)                                                  M24. 275
      REALZ = DREAL ( X )                                               M31. 154
      RETURN                                                            M24. 278
      ENTRY CABSZ(X)                                                    M24. 279
      CABSZ = CDABS ( X )                                               M31. 155
      RETURN                                                            M24. 282
      END                                                               M19. 232
      SUBROUTINE CMULTZ (AR,AI,BR,BI,CR,CI,KSN)                         M27. 350
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      SR=BR*CR-BI*CI                                                    M27. 351
      AI=BI*CR+BR*CI                                                    M27. 352
      AR=SR                                                             M27. 353
      IF (KSN.GE.0) GO TO 200                                           M27. 354
      AR=-AR                                                            M27. 355
      AI=-AI                                                            M27. 356
  200 RETURN                                                            M27. 357
      END                                                               M27. 358
      SUBROUTINE CDIVZ (AR,AI,BR,BI,CR,CI,KSN)                          M27. 359
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      FAC=CR*CR+CI*CI                                                   M27. 360
      FAC=1./FAC                                                        M27. 361
      SR=BR*FAC*CR+BI*FAC*CI                                            M27. 362
      AI=BI*FAC*CR-BR*FAC*CI                                            M27. 363
      AR=SR                                                             M27. 364
      IF (KSN.GE.0) GO TO 200                                           M27. 365
      AR=-AR                                                            M27. 366
      AI=-AI                                                            M27. 367
  200 RETURN                                                            M27. 368
      END                                                               M27. 369
      FUNCTION  IABSZ ( N1 )                                            M21.   8
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     ONE AND ONLY INTEGER LIBRARY FUNCTION OF ONE INTEGER              M28. 200
C     ARGUMENT.    MAKE ENTRY POINT IF 2ND IS USED.                     M28. 201
      IABSZ = IABS ( N1 )                                               M21.   9
      RETURN                                                            M21.  10
      END                                                               M21.  11
      FUNCTION IFUNL2 ( N1, N2 )                                        M25.  77
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     PROVISION FOR ALL INTEGER LIBRARY FUNCTIONS OF 2 INTEGER ARGUMENTSM25.  78
      IFUNL2 = N1                                                       M25.  81
      RETURN                                                            M25.  82
      ENTRY ISIGNZ (N1, N2)                                             M25.  83
      ISIGNZ = ISIGN (N1, N2)                                           M25.  84
      RETURN                                                            M25.  85
      ENTRY MODZ (N1, N2)                                               M25.  86
      MODZ = MOD (N1, N2)                                               M27. 370
      RETURN                                                            M25.  88
      ENTRY MIN0Z (N1, N2)                                              M25.  89
      MIN0Z = MIN0 (N1, N2)                                             M25.  90
      RETURN                                                            M25.  91
      ENTRY MAX0Z (N1, N2)                                              M25.  92
      MAX0Z = MAX0 (N1, N2)                                             M25.  93
      RETURN                                                            M25.  94
      END                                                               M25.  95
      SUBROUTINE  DLIBRF(X, Y)                                          M32. 169
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DOUBLE PRECISION X, Y                                             M32. 170
C     THIS MODULE SERVES TO PROVIDE SELECTED DOUBLE-PRECISION           M32. 171
C     LIBRARY FUNCTIONS FOR SEVERAL PLACES IN THE PROGRAM.              M32. 172
C     MAKING THIS A SUBROUTINE RATHER THAN A FUNCTION AVOIDS ALL        M32. 173
C     COMPLICATIONS WITH THE MODULE NAME HAVING A VARIABLE TYPE         M32. 174
C     ASSOCIATED WITH IT.   IT IS INSTALLATION-DEPENDENT BECAUSE OF TWO M32. 175
C     THINGS --- FIRST THE USE OF ENTRY POINTS, AND SECOND THE USE      M32. 176
C     OF THE DOUBLE-PRECISION DECLARATION (BY WHICH IS MEANT DOUBLE     M32. 177
C     THE PRECISION OF REGULAR FLOATING-POINT VARIABLES OF THE EMTP).   M32. 178
C     SINCE MOST BYTE-ORGANIZED MACHINES USE  REAL*8  FOR OTHER         M32. 179
C     VARIABLES, THIS IMPLIES  REAL*16 ,  IF AVAILABLE.                 M32. 180
C     INSTALLATION-DEPENDENT MODULE CODED FOR  DEC VAX-11               M32. 181
      RETURN                                                            M32. 182
      ENTRY DABSZ (x, y)                                                M32. 183
      Y = DABS(X)                                                       M32. 184
      RETURN                                                            M32. 185
      ENTRY DCOSZ (x, y)                                                M32. 186
      Y = DCOS(X)                                                       M32. 187
      RETURN                                                            M32. 188
      ENTRY DEXPZ (x, y)                                                M32. 189
      Y = DEXP(X)                                                       M32. 190
      RETURN                                                            M32. 191
      ENTRY DSINZ (x, y)                                                M32. 192
      Y = DSIN(X)                                                       M32. 193
      RETURN                                                            M32. 194
      ENTRY DSQRTZ (x, y)                                               M32. 195
      Y = DSQRT(X)                                                      M32. 196
      RETURN                                                            M32. 197
      ENTRY DLOGZ (x, y)                                                M35. 270
      Y = DLOG(X)                                                       M33.  64
      RETURN                                                            M33.  65
      END                                                               M32. 198
      SUBROUTINE DLIBR2 ( X, Y, Z )                                     M32. 199
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DOUBLE PRECISION  X, Y, Z                                         M35. 271
C     INSTALLATION-DEPENDENT MODULE FOR DEC VAX-11 COMPUTER             M32. 200
C     LIKE "DLIBRF" (SEE COMMENTS THERE), ONLY FOR TWO INPUTS           M32. 201
      RETURN                                                            M32. 202
      ENTRY DATN2Z (x,y,z)                                              M32. 203
      Z = DATAN2 ( X, Y )                                               M33.  66
      RETURN                                                            M32. 205
      END                                                               M32. 206
      SUBROUTINE SETMAR                                                 M32. 207
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     SUBROUTINE TO CHANGE PAGE SIZE AND EJECT PAGE FOR PRINTER PLOTS.  M32. 208
C     DUMMY MODULE SINCE THIS ONTARIO HYDRO (UNIVAC) TRICK IS UNKNOWN.  M32. 209
      RETURN                                                            M32. 211
      ENTRY  CHRSIZ ( N )                                               M32. 212
C     ENTRY POINT FOR DO-NOTHING MIMICED TEKTRONIX PLOT10 OF "TEKPLT"   M32. 213
      RETURN                                                            M32. 218
      ENTRY  SETPLT                                                     M32. 212
C     ENTRY POINT TO CHANGE LINES/PAGE TO MAXIMUM NUMBER ALLOWED,       M32. 213
C     TO ALLOW PRINTER PLOTS TO BE CONTINUOUS OVER PAGE BOUNDRIES.      M32. 214
C     CALL SYSTEM DEPENDANT ROUTINE TO CHANGE PAGE SIZE                 M32. 215
C     WRITE (LUNIT6, 1000 )                                             M32. 216
 1000 FORMAT ( 1H1 )                                                    M32. 217
      RETURN                                                            M32. 218
      ENTRY  SETSTD                                                     M32. 219
C     ENTRY POINT TO RESTORE PAGE LIMITS TO STANDARD VALUES.            M32. 220
C     CALL SYSTEM DEPENDANT ROUTINE TO CHANGE PAGE SIZE                 M32. 221
C     WRITE (LUNIT6, 1000 )                                             M32. 222
      RETURN                                                            M32. 223
      END                                                               M32. 224
      SUBROUTINE INTERP                                                 M35. 272
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     IF  CHARACTER*51 KUNIT6,  ACTIVE MODULE NEEDED TO FLUSH           M35. 273
C     ABUFF AND KUNIT6 AS 132-COLUMN INTERPRETED DATA CARD LINE.        M35. 274
      RETURN                                                            M35. 275
      END                                                               M35. 276
      SUBROUTINE MOVER( A, B, N )                                       M40.  95
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    SUBROUTINE  MOVER  (WITH ENTRY POINT MOVER0) IS USED FOR BLOCK    M15. 297
C)    TRANSFERS BETWEEN CONTIGUOUS CELLS OF CORE STORAGE.   'N'  IS THE M15. 298
C)    NUMBER OF WORDS TO BE TRANSFERED.                                 M15. 299
C)         1.  USING 'MOVER', THE TRANSFER IS FROM  A(J)  TO  B(J),     M15. 300
C)             FOR  J=1, N.                                             M15. 301
C)         2.  USING  'MOVER0',  A(1)  IS COPIED INTO ALL  N  CELLS     M15. 302
C)             OF ARRAY  'B'.   FOR ZEROING ARRAY  'B' ,  THE SUBROUTINEM15. 303
C)             CALL IS MADE WITH THE FIRST ARGUMENT EXPLICITELY         M15. 304
C)             PUNCHED AS ZERO.                                         M15. 305
      DIMENSION  A(1), B(1)                                             M40.  96
      DO 1000  I=1, N                                                   M40.  97
 1000 B(I) = A(I)                                                       M40.  98
      RETURN                                                            M40.  99
      END                                                               M40. 100
      SUBROUTINE MOVER0 ( B, N )                                        M40. 101
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION B(1)                                                    M40. 102
C)    SUBROUTINE  MOVER0  IS A BLOCK-ZEROING ROUTINE FOR FLOATING-POINT M15. 306
C)    ARRAYS OR VARIABLES.   OF THE TWO ARGUMENTS, THE SECOND,  'N' ,   M15. 307
C)    IS THE NUMBER OF WORDS OF CORE TO BE ZEROED.   THE FIRST ARGUMENT M15. 308
C)    SPECIFIES THE ADDRESS OF THE FIRST WORD OF THE BLOCK OF  'N'      M15. 309
C)    WORDS OF CORE WHICH IS TO BE ZEROED.                              M15. 310
      DO 1100  I=1, N                                                   M40. 103
 1100 B(I) = 0.0                                                        M40. 104
      RETURN                                                            M40. 105
      END                                                               M40. 106
      SUBROUTINE MOVE ( INTA, INTB, N )                                 M42. 625
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    SUBROUTINE  MOVE  IS IDENTICAL TO THE                             M15. 311
C)    BLOCK-TRANSFER ROUTINE  MOVER  EXCEPT                             M15. 312
C)    THAT  MOVE  IS FOR INTEGER ARRAYS, WHILE  MOVER  WAS FOR          M15. 313
C)    FLOATING-POINT ARRAYS.   THERE IS A DIFFERENCE, OF COURSE, ON     M15. 314
C)    MACHINES LIKE IBM, WHERE INTEGER WORDS MAY BE SHORTER THAN        M15. 315
C)    FLOATING-POINT WORDS.                                             M15. 316
      DIMENSION INTA(1), INTB(1)                                        M42. 626
      DO 1000  I=1, N                                                   M42. 627
 1000 INTB(I) = INTA(I)                                                 M42. 628
      RETURN                                                            M42. 629
      END                                                               M42. 630
      SUBROUTINE MOVE0 ( INTB, N )                                      M42. 631
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    SUBROUTINE  MOVE0  IS IDENTICAL TO  THE BLOCK-ZEROING ROUTINE     M15. 317
C)    MOVER0  EXCEPT THAT  MOVE0  IS FOR INTEGER ARRAYS, WHILE  MOVER0  M15. 318
C)    IS FOR FLOATING-POINT ARRAYS.   THERE IS A DIFFERENCE, ON         M15. 319
C)    MACHINES LIKE IBM, WHERE INTEGER WORDS MAY BE SHORTER THAN        M15. 320
C)    FLOATING-POINT WORDS.                                             M15. 321
      DIMENSION INTB(1)                                                 M42. 632
      DO 1100  I=1, N                                                   M42. 633
 1100 INTB(I) = 0                                                       M42. 634
      RETURN                                                            M42. 635
      END                                                               M42. 636
      SUBROUTINE ADDMXD( A, B, C, N )                                   M42. 637
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    SUBROUTINE  ADDMXD  FORMS MATRIX   (C) = (A) + B(U)  , WHERE (A), M15. 322
C)    AND (C)  ARE N BY N MATRICES,  B  IS A SCALAR, AND (U) IS THE     M15. 323
C)    IDENTITY MATRIX.   ARRAY (C) MAY BE THE SAME AS (A), IF DESIRED.  M15. 324
C)    SEE SUBR.  MULT  FOR SYMMETRIC-MATRIC STORAGE SCHEME ASSUMED.     M15. 325
      DIMENSION  A(1), C(1)                                             M42. 638
      K = 1                                                             M42. 639
      J = 1                                                             M42. 640
      JT = N * (N+1) / 2                                                M42. 641
      DO 3010  L=1, JT                                                  M42. 642
      C(L) = A(L)                                                       M42. 643
      IF( L .LT. K )  GO TO 3010                                        M42. 644
      C(L) = C(L) + B                                                   M42. 645
      J = J + 1                                                         M42. 646
      K = K + J                                                         M42. 647
 3010 CONTINUE                                                          M42. 648
      RETURN                                                            M42. 649
      END                                                               M42. 650
      SUBROUTINE MULTMX( A, B, C, TEMP, N )                             M42. 651
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    SUBROUTINE MULTMX  FORMS THE MATRIX PRODUCT   (C) = (A)(B)   WHEREM15. 326
C)    MATRICES  (A), (B), AND (C)  ARE ALL  N BY N  SQUARE ARRAYS.      M15. 327
C)    ARRAY  'TEMP'  IS A SCRATCH WORKING AREA OF NOT LESS THAN  2N     M15. 328
C)    CELLS.   ARRAYS  (B)  AND  (C)  MAY BE IDENTICAL, THEREBY PLACING M15. 329
C)    THE PRODUCT   (A)(B)   BACK INTO  (B) .    SEE SUBROUTINE  'MULT' M15. 330
C)    WHICH IS CALLED HEREIN, FOR DETAILS ABOUT THE STORAGE SCHEME USED M15. 331
C)    FOR THESE REAL, SYMMETRIC MATRICES.                               M15. 332
      DIMENSION A(1), B(1), C(1), TEMP(1)                               M42. 652
      LL0 = 0                                                           M22. 673
      II = 0                                                            M42. 653
      DO 3450  J=1, N                                                   M42. 654
      DO 3430  I=1, N                                                   M42. 655
      IF( I .LE. J )  GO TO 3420                                        M42. 656
      L = L + (I-1)                                                     M42. 657
      GO TO 3430                                                        M42. 658
 3420 L = II + I                                                        M42. 659
 3430 TEMP(I) = B(L)                                                    M42. 660
      M = N + 1                                                         M42. 661
      CALL MULT ( A(1), TEMP(1), TEMP(M), N, LL0 )                      M22. 674
      CALL MOVER( TEMP(M), C(II+1), J )                                 M42. 662
      II = II + J                                                       M42. 663
 3450 CONTINUE                                                          M42. 664
      RETURN                                                            M42. 665
      END                                                               M42. 666
      SUBROUTINE FREFLD ( ARRAY )                                       M20. 235
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      REAL*8         TEXT1, CHTACS, TEXBUF, TEXVEC                      M29. 930
      DIMENSION  TEXBUF(30), ARRAY(1), TEXVEC(1)                        M29. 931
      EQUIVALENCE  ( TEXVEC(1),  TEXT1 )                                M29. 932
      DATA  CHTACS  /  6HTACS     /                                     M28. 213
      IF ( IPRSUP  .GE.  5 )                                            M32. 225
     1 WRITE (LUNIT6, 1016)  NFRFLD, NRIGHT, KOLBEG                     M32. 226
 1016 FORMAT ( 40H TOP "FREFLD".  NFRFLD, NRIGHT, KOLBEG =,             M32. 227
     1  3I6  )                                                          M32. 228
      IF ( NRIGHT  .LT.  0 )   GO TO 5913                               M20. 261
      DO 5851  JJ=1, NFRFLD                                             M20. 262
      IF ( KOLBEG  .LE.  80 )   GO TO 5600                              M20. 263
      LSTAT(19) = 5600                                                  M20. 264
      GO TO 9200                                                        M20. 265
 5600 N3 = 0                                                            M28. 214
      GO TO 5805                                                        M20. 274
 5603 IF ( CHCONT  .EQ.  CHTACS )   GO TO 5614                          M28. 215
      IF ( TEXT1  .EQ.  BLANK )   GO TO 5802                            M28. 216
      IF ( TEXT1  .NE.  CSEPAR )   GO TO 5623                           M20. 276
 5609 KOLBEG = KOLBEG + 1                                               M28. 217
      GO TO 5827                                                        M20. 278
 5614 IF ( TEXT1  .NE.  CSEPAR )   GO TO 5623                           M28. 218
      IF ( TEXT1  .EQ.  BLANK )   GO TO 5802                            M28. 219
      GO TO 5609                                                        M28. 220
 5623 IF ( N3  .LT.  30 )   GO TO 5627                                  M32. 229
      LSTAT(19) = 5623                                                  M32. 230
      GO TO 9200                                                        M32. 231
 5627 N3 = N3 + 1                                                       M32. 232
      TEXBUF(N3) = TEXT1                                                M28. 223
 5802 KOLBEG = KOLBEG + 1                                               M20. 322
 5805 TEXT1 = TEXCOL(KOLBEG)                                            M20. 323
      IF ( TEXT1  .NE.  CHCONT )   GO TO 5819                           M20. 324
C     READ INPUT CARD USING CIMAGE                                      M20. 325
      CALL CIMAGE                                                       M20. 326
      KOLBEG = 1                                                        M20. 327
      IF ( N3  .EQ.  0 )   GO TO 5805                                   M20. 328
      GO TO 5827                                                        M20. 329
 5819 IF ( KOLBEG  .LE.  80 )   GO TO 5603                              M20. 330
 5827 IF ( N3  .GT.  0 )   GO TO 5829                                   M28. 224
      ARRAY(JJ) = 0.0                                                   M28. 225
      GO TO 5831                                                        M28. 226
 5829 CALL FRENUM ( TEXBUF(1), N3, ARRAY(JJ) )                          M28. 227
 5831 IF ( IPRSUP  .GE.  5 )                                            M28. 228
     1 WRITE (LUNIT6, 5837)  JJ, KOLBEG, N3, ARRAY(JJ)                  M28. 229
 5837 FORMAT ( /,   17H "FREFLD" NUMBER.,                               M28. 230
     1  24H      JJ  KOLBEG      N3,  21X,  9HARRAY(JJ)  ,/,            M28. 231
     2  17X,  3I8,  E30.20  )                                           M28. 232
 5851 CONTINUE                                                          M20. 349
      GO TO 9900                                                        M32. 233
 5913 IF ( NRIGHT  .LT.  -1 )   GO TO 6036                              M22. 676
      DO 5948  JJ=1, NFRFLD                                             M22. 677
      TEXTA6(JJ) = BLANK                                                M28. 233
      LL = 0                                                            M22. 678
      IF ( KOLBEG  .LE.  80 )   GO TO 5920                              M22. 679
      LSTAT(19) = 5920                                                  M22. 680
      GO TO 9200                                                        M22. 681
 5920 TEXT1 = TEXCOL(KOLBEG)                                            M22. 682
      KOLBEG = KOLBEG + 1                                               M22. 683
      IF ( CHCONT  .EQ.  CHTACS )   GO TO 5928                          M28. 234
      IF ( TEXT1  .EQ.  BLANK )   GO TO 5923                            M28. 235
      IF ( TEXT1  .EQ.  CSEPAR )   GO TO 5948                           M22. 685
 5921 IF ( LL  .LE.  6 )   GO TO 5922                                   M28. 236
      LSTAT(19) = 5922                                                  M22. 687
      GO TO 9200                                                        M22. 688
 5928 IF ( TEXT1  .EQ.  CSEPAR )   GO TO 5948                           M28. 237
      IF ( TEXT1  .EQ.  BLANK )   GO TO 5923                            M28. 238
      GO TO 5921                                                        M28. 239
 5922 LL = LL + 1                                                       M22. 689
      CALL PACKA1 ( TEXVEC(1), TEXTA6(JJ), LL )                         M29. 933
 5923 IF ( KOLBEG  .LE.  80 )   GO TO 5920                              M22. 691
 5948 CONTINUE                                                          M20. 366
      GO TO 9900                                                        M32. 234
 6036 LL = 0                                                            M22. 692
      JJ = 0                                                            M22. 693
      GO TO 6054                                                        M22. 694
 6042 JJ = JJ + 1                                                       M22. 695
      IF ( JJ  .GT.  10 )   GO TO 6072                                  M28. 240
      TEXTA6(JJ) = BLANK                                                M28. 241
      LL = 0                                                            M22. 700
 6048 TEXT1 = TEXCOL(KOLBEG)                                            M22. 701
      IF ( CHCONT  .EQ.  CHTACS )   GO TO 6051                          M28. 242
      IF ( TEXT1  .EQ.  BLANK )   GO TO 6054                            M28. 243
      IF ( TEXT1  .EQ.  CSEPAR )   GO TO 6072                           M22. 703
      GO TO 6052                                                        M28. 244
 6051 IF ( TEXT1  .EQ.  CSEPAR )   GO TO 6072                           M28. 245
      IF ( TEXT1  .EQ.  BLANK )   GO TO 6054                            M28. 246
 6052 IF ( LL  .EQ.  6 )   GO TO 6042                                   M28. 247
      LL = LL + 1                                                       M22. 705
      CALL PACKA1 ( TEXVEC(1), TEXTA6(JJ), LL )                         M29. 934
      KOLBEG = KOLBEG + 1                                               M22. 707
      GO TO 6048                                                        M22. 708
 6054 N9 = KOLBEG                                                       M22. 709
      DO 6059  I=KOLBEG, 80                                             M22. 710
      IF ( TEXCOL(I)  .NE.  BLANK )   GO TO 6067                        M28. 248
 6059 CONTINUE                                                          M22. 712
      KOLBEG = 79                                                       M22. 713
      GO TO 6072                                                        M22. 714
 6067 KOLBEG = I                                                        M22. 715
      IF ( KOLBEG - N9   .LE.   2 )   GO TO 6069                        M22. 716
      IF ( JJ  .GT.  0 )   GO TO 6072                                   M22. 717
 6069 IF ( TEXCOL(KOLBEG)  .NE.  CSEPAR )   GO TO 6042                  M22. 718
 6072 NFRFLD = JJ                                                       M22. 719
      KOLBEG = KOLBEG + 1                                               M22. 720
      IF ( IPRSUP  .GE.  1 )                                            M22. 721
     1 WRITE (LUNIT6, 6083)  JJ, LL, KOLBEG, TEXCOL                     M22. 722
 6083 FORMAT ( /,  28H KEYWORD NEAR "FREFLD" EXIT.,                     M32. 235
     1 24H      JJ      LL  KOLBEG   ,/,  28X,  3I8  ,/,                M32. 236
     2   ( 9H TEXCOL =,  30A4 )  )                                      M32. 237
      GO TO 9900                                                        M32. 238
 9200 KILL = 166                                                        M22. 726
      IF ( IPRSUP  .GE.  0 )                                            M22. 727
     1 WRITE (LUNIT6, 9207)  LSTAT(19), NCHAIN, LASTOV, KOLBEG,         M22. 728
     1                       NFRFLD, NRIGHT                             M22. 729
 9207 FORMAT ( /,  30H ERROR STOP WITHIN  'FREFLD' .,  6I8,/, 1X )      M22. 730
      LSTAT(18) = -1                                                    M20. 369
 9900 IF ( IPRSUP  .GE.  2 )                                            M32. 239
     1 WRITE (LUNIT6, 9901)  KILL, KOLBEG, ARRAY(1)                     M32. 240
 9901 FORMAT ( 41H EXIT "FREFLD".  KILL, KOLBEG, ARRAY(1) =,            M32. 241
     1  2I6,  E20.10  )                                                 M32. 242
      RETURN                                                            M32. 243
      END                                                               M20. 371
      SUBROUTINE FREONE ( D1 )                                          M29. 935
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     SCALAR VERSION OF  "FREFLD"  WHICH ENTERS THE UTPF WITH           M29. 936
C     "M29."  VINTAGE, TO SATISFY BURROUGHS (SEE PROBLEM B,             M29. 937
C     SECTION II, PAGE ECWB-4, VOL. X  EMTP MEMO OF 14 FEB 1981.)       M31. 156
      DIMENSION  ARRAY(1)                                               M29. 939
      CALL FREFLD ( ARRAY(1) )                                          M29. 940
      D1 = ARRAY(1)                                                     M29. 941
      RETURN                                                            M29. 942
      END                                                               M29. 943
      SUBROUTINE FRENUM ( TEXT1, N3, D1 )                               M28. 249
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     VAX-11/780  INSTALLATION-DEPENDENT MODULE CALLED ONLY BY          M28. 250
C     THE FREE-FORMAT DATA MODULE  "FREFLD" .   PURPOSE IS TO           M28. 251
C     CONVERT INPUT CHARACTERS  (TEXT1(1) ... TEXT1(N3))  INTO          M28. 252
C     A FLOATING POINT NUMBER.                                          M28. 253
      REAL*8        TEXT1(1), BLANK                                     M28. 254
      LOGICAL*1  TEXTA(30), TEXTB                                       M29. 944
      DATA  BLANK   /  6H          /                                    M28. 256
      DATA  TEXTB   /  1H          /                                    M28. 257
      N9 = 30                                                           M28. 258
      N4 = N3 + 1                                                       M28. 259
      DO 4718  I=1, N3                                                  M28. 260
      N4 = N4 - 1                                                       M28. 261
      IF ( TEXT1(N4)  .EQ.  BLANK )   GO TO 4718                        M28. 262
      IF ( N9  .GE.  2 )   GO TO 4711                                   M28. 263
      WRITE (6, 4706)                                                   M30. 104
 4706 FORMAT ( /, 34H ERROR STOP IN "FRENUM". THERE ARE,                M32. 244
     1            33H 33 OR MORE CHARACTERS IN A FREE-,                 M32. 245
     2            33H FORMAT NUMBER ON LAST DATA CARD.   )              M32. 246
      CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD          M37. 895
 4711 ENCODE (1, 4712, TEXTA(N9))  TEXT1(N4)                            M28. 269
 4712 FORMAT ( 80A1 )                                                   M28. 270
      N9 = N9 - 1                                                       M28. 271
 4718 CONTINUE                                                          M28. 272
      DO 4726  I=1, N9                                                  M28. 273
 4726 TEXTA(I) = TEXTB                                                  M28. 274
      DECODE (30, 4732, TEXTA(1))  D1                                   M28. 275
 4732 FORMAT ( E30.0 )                                                  M28. 276
      RETURN                                                            M28. 277
      END                                                               M28. 278
      SUBROUTINE  PACKA1 ( FROM, TO, KK )                               M20. 372
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     SYSTEM-DEPENDENT EMTP MODULE  'PACKA1'  FOR  VAX-11/780.          M28. 279
C     ARGUMENT  'FROM'  CONTAINS  A1  INFORMATION WHICH IS TO BE STORED M20. 374
C     IN CHARACTER POSITION  KK  OF ARGUMENT  'TO' .                    M20. 375
C     FOR ALL EMTP USAGE,  1ST 2 ARGUMENTS MUST BE VECTORS.             M29. 945
      LOGICAL*1   FROM(1), TO(6)                                        M29. 946
      TO(KK) = FROM(1)                                                  M29. 947
      RETURN                                                            M20. 383
      END                                                               M20. 384
      SUBROUTINE PACKCH ( FROM, TO, K4OR6, NCHBEG, NWORD )              M23.  40
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)                                                                      M23.  41
C)    THIS MODULE PERFORMS THE SYSTEM-DEPENDENT FUNCTION OF PACKING BCD M23.  42
C)    CHARACTERS FROM  A4  OR  A6  WORDS TOGETHER SO AS TO FORM A       M23.  43
C)    CONTIGUOUS STRING OF CHARACTERS WITHOUT EXTRA BLANK FILL.   FOR   M23.  44
C)    EXAMPLE, CDC HAS A 60-BIT WORD WHICH STORES 10 BCD CHARACTERS (6  M23.  45
C)    BITS TO A CHARACTER).   WITH PLOT ALPHANUMERIC TEXT READ UNDER  A6M23.  46
C)    FORMAT CONTROL, THE RIGHT-MOST FOUR CHARACTERS OF SUCH WORDS WILL M23.  47
C)    BE BLANK-FILLED BY THE SYSTEM.   IBM'S EIGHT CHARACTERS IN AN     M23.  48
C)    EIGHT-BYTE WORD POSE AN ANALOGOUS PROBLEM.   SINCE THE  CALCOMP   M23.  49
C)    PLOTTING SUBROUTINE CALLS WHICH APPEAR IN                         M23.  50
C)    THE CALLING MODULE REQUIRE CONTIGUOUS CHARACTER STRINGS, OUTPUT   M23.  51
C)    TEXT MUST BE COMPRESSED SO AS TO REMOVE THE WORD-LENGTH-DEPENDENT M23.  52
C)    BLANK FILL.   THE PRESENT SUBROUTINE IS CALLED BY THE PLOTTING    M23.  53
C)    CODE TO PERFORM THIS FUNCTION.   MEANING OF THE SUBROUTINE        M23.  54
C     ARGUMENTS IS AS FOLLOWS ......                                    M23.  55
C      NWORD ----- THE NUMBER OF WORDS WHICH ARE TO HAVE THEIR BCD      M23.  56
C                  CHARACTERS EXTRACTED, AND PACKED INTO  A CHARACTER   M23.  57
C                  STRING.   THIS IS A POSITIVE INTEGER.                M23.  58
C      FROM(1)  ----- THE FIRST OF  'NWORD'  WORDS  WHOSE BCD CONTENTS  M23.  59
C                     ARE TO BE PACKED.                                 M23.  60
C      K4OR6  ----- EQUAL TO EITHER  4  OR  6 ,  WHICHEVER IS THE NUMBERM23.  61
C                   OF CHARACTERS OF BCD INFORMATION WHICH IS STORED IN M23.  62
C                   THE WORDS   (FROM(I), I=1, NWORD) .                 M23.  63
C      TO(1)  ----- THE ACTIVE BCD CHARACTERS OF   (FROM(I), I=1, NWORD)M23.  64
C                   ARE TO BE PACKED AS A CONTIGUOUS STRING OF          M23.  65
C                   CHARACTERS BEGINNING WITH CHARACTER POSITION        M23.  66
C                   'NCHBEG'  OF WORD  TO(1) .                          M23.  67
C      NCHBEG  -----  THE CHARACTER POSITION OF WORD  TO(1)  WHERE THE  M23.  68
C                     CONTIGUOUS STRING OF BCD CHARACTERS IS TO BEGIN.  M23.  69
C                     NORMALLY  'NCHBEG'  IS BETWEEN  1  AND  10 ,      M23.  70
C                     ALTHOUGH LARGER POSITIVE VALUES ARE ALLOWED.      M23.  71
C                     FOR EXAMPLE, A VALUE OF  27  MEANS THAT THE CDC   M23.  72
C                     CHARACTER INSERTION BEGINS IN POSITION  7  OF     M23.  73
C                     WORD  TO(3) .                                     M23.  74
      LOGICAL*1        FROM(NWORD), TO(NWORD)                           M27. 379
      ICHAR = NCHBEG                                                    M24. 286
      DO 120  IWORD=1, NWORD                                            M24. 287
      DO 110  KCHAR=1, K4OR6                                            M24. 288
      JCHAR = (IWORD-1)*8 + KCHAR                                       M24. 289
      TO(ICHAR) = FROM(JCHAR)                                           M24. 290
      ICHAR = ICHAR + 1                                                 M24. 291
  110 CONTINUE                                                          M24. 292
  120 CONTINUE                                                          M24. 293
      RETURN                                                            M24. 294
      END                                                               M23. 119
      FUNCTION SEEDY (ATIM)                                             M23. 120
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C                                                                       M23. 121
C)    THIS FUNCTION IS DESIGNED TO TAKE THE TIME OF DAY (WALL-CLOCK     M23. 122
C)    TIME) IN BCD FORM AS INPUT, AND RETURN THE NUMBER OF SECONDS      M23. 123
C)    SINCE MIDNIGHT AS OUTPUT.   THE TIME OF DAY IS ASSUMED TO BE      M23. 124
C)    CODED IN WORDS   ATIM(1)   AND   ATIM(2) ,   AS FOUND BY A CALL TOM23. 125
C)    SUBROUTINE  'TIME44' .   THE STORAGE HERE IS IN FORMAT   2A4 ,    M23. 126
C)    WITH THE FIRST FOUR CHARACTERS OF   'HH.MM.SS'   ASSUMED TO BE    M23. 127
C)    IN  ATIM(1) ,  AND THE LAST FOUR IN  ATIM(2) .                    M24. 295
C)                                                                      M23. 132
      REAL*8          ATIM                                              M23. 133
      DIMENSION ATIM(2)                                                 M23. 134
      DECODE (12, 4286, ATIM )   IHR, IMIN10, IMIN1, ISEC               M28. 280
 4286 FORMAT ( I2, 1X, I1, 4X, I1, 1X, I2 )                             M24. 297
      IMIN = IMIN10 * 10  +  IMIN1                                      M23. 137
      HOUR = IHR * 3600                                                 M23. 138
      AMIN = IMIN * 60                                                  M23. 139
      SEC = ISEC                                                        M23. 140
      SEEDY = SEC  +  AMIN  +  HOUR  +  1.0                             M23. 141
      RETURN                                                            M23. 142
      END                                                               M23. 143
      FUNCTION RANDNM (X)                                               M23. 144
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)                                                                      M23. 145
C)    THIS IS A RANDOM NUMBER GENERATING FUNCTION, WHOSE PURPOSE IS TO  M23. 146
C)    RETURN WITH A VALUE FOR A RANDOM VARIABLE WHICH IS UNIFORMLY-     M23. 147
C)    DISTRIBUTED OVER THE INTERVAL FROM ZERO TO UNITY.                 M23. 148
C)    A TYPICAL CALL IS    Y = RANDNM(X)  ,    WHERE  'X'  IS A         M23. 149
C)    FLOATING-POINT VARIABLE WHICH IS PRESENT ONLY BECAUSE 'RANDOM'    M23. 150
C)    NUMBER-GENERATING ALGORITHMS ARE NEVER TRULY RANDOM, AND GENERALLYM23. 151
C)    REQUIRE SOME SORT OF 'RANDOM' INITIALIZATION TO EVEN GET STARTED. M23. 152
C)    SINCE SUCH NUMBER-GENERATING ALGORITHMS ARE ACTUALLY CYCLIC IF A  M23. 153
C)    LARGE ENOUGH SAMPLING IS TAKEN, IT IS ALSO DESIRABLE TO           M23. 154
C)    RE-INITIALIZE THE ALGORITHM PERIODICALLY, BY MEANS OF A 'RANDOM'  M23. 155
C)    EXTERNAL INPUT.   VARIABLE  'X'  IS THIS 'RANDOM' INPUT, WHOSE    M23. 156
C)    PURPOSE IS TO RANDOMLY RE-INITIALIZE THE ALGORITHM, AS FOLLOWS ...M23. 157
C)         1.  IF  'X'  IS IDENTICALLY ZERO, THERE IS NO INITIALIZATION.M23. 158
C)             THE NEXT RANDOM NUMBER IS SIMPLY RETURNED THROUGH THE    M23. 159
C)             FUNCTION NAME.                                           M23. 160
C)         2.  IF  'X'  IS POSITIVE, THE RANDOM NUMBER GENERATING       M23. 161
C)             ALGORITHM IS TO BE INITIALIZED.   EMTP USAGE HAS  'X'    M28. 281
C)             AS THE WALL-CLOCK TIME IN SECONDS SINCE MIDNIGHT, AN     M23. 163
C)             INTEGER (THOUGH STORED IN FLOATING-POINT MODE, NOTE).    M23. 164
C)             IN THIS CASE, NO RANDOM NUMBER NEED BE RETURNED WITH THE M23. 165
C)             FUNCTION NAME, SINCE THE EMTP WILL NOT USE IT.           M28. 282
C)    IF A NON-CDC USER HAS ACCESS TO A RANDOM NUMBER GENERATING        M23. 167
C)    FUNCTION WHICH DOES NOT REQUIRE INITIALIZATION, HE MAY SIMPLY     M23. 168
C)    IGNORE  'X' ,   AND RETURN THE RANDOM NUMBER THROUGH THE FUNCTION M23. 169
C)    NAME EVERY TIME THIS MODULE IS CALLED.                            M23. 170
C)                                                                      M23. 171
C     A MINUS SIGN APPENDED TO VARIABLE  'XMAXMX'  OF   /BLANK/  IS A   M27. 380
C     FLAG THAT THE USER WANTS TO EMPLOY THE STANDARD TABLE OF RANDOM   M27. 381
C     NUMBERS WHICH IS BUILT INTO MODULE  'SANDNM' .                    M27. 382
C                                                                       M27. 383
C     INSTALLATION-DEPENDENT  EMTP  MODULE WRITTEN FOR THE  DEC         M24. 298
C     VAX-11/780.    'RAN'  IS A  DEC  SYSTEM SUBROUTINE WHICH          M24. 299
C     RETURNS A RANDOM NUMBER UNIFORMLY DISTRIBUTED OVER  (0, 1) .      M24. 300
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      EQUIVALENCE (MONCAR(1), KNT)                                      M39.  54
      IF ( XMAXMX  .LT.  0.0 )   GO TO 7265                             M27. 385
      IF ( X  .EQ.  0.0 )   GO TO 4213                                  M24. 306
      IF ( KNT .GT. 1 )  GO TO 9800   ! SKIP 2ND OR LATER SEED          M39.  55
      N14 = X                                                           VAX.  43
      IF ( N14/2*2 .EQ. N14 ) N14 = N14 + 1                             VAX.  44
 4213 RANDNM = RAN( N14 )  ! 29 Dec 1987, change from  (N1, N2)         M24. 316
      GO TO 9800                                                        M39.  56
 7265 RANDNM = SANDNM(X)                                                M27. 386
 9800 RETURN                                                            M39.  57
      END                                                               M23. 183
      FUNCTION  SANDNM ( X )                                            M27. 388
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    THIS VERSION OF  'RANDNM'  IS USED FOR TESTING OF THE             M27. 389
C)    STATISTICAL OVERVOLTAGE CAPABILITY OF THE EMTP ONLY.   IT USES    M27. 390
C)    BUILT-IN RANDOM NUMBERS, SO AS TO PRODUCE IDENTICAL RESULTS       M27. 391
C)    ON ALL COMPUTER SYSTEMS, THEREBY PERMITTING COMPARATIVE           M27. 392
C)    VERIFICATION OF RESULTS EXACTLY AS FOR CONVENTIONAL TEST CASES.   M27. 393
C)    IN ORDER TO AVOID RE-USE OF RANDOM NUMBERS, THE NUMBER OF         M27. 394
C)    ENERGIZATIONS  'NENERG'  TIMES THE NUMBER OF SWITCHES  'KSWTCH'   M27. 395
C)    MUST NOT EXCEED THE DIMENSION OF ARRAY  'A' .                     M27. 396
C     IF  'STATISTICS'  MISCELLANEOUS DATA PARAMETER  'XMAXMX'          M27. 397
C     IS INPUT BY THE USER WITH NEGATIVE VALUE, THE EMTP TAKES          M27. 398
C     THIS TO MEAN THAT "SANDNM" IS TO BE USED FOR RANDOM               M27. 399
C     NUMBERS RATHER THAN "RANDNM" .                                    M27. 400
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      DIMENSION  A(100)                                                 M27. 402
      EQUIVALENCE  ( MONCAR(1), KNT )                                           
C     BURROUGHS: PRESERVE LOCAL VARIABLE BETWEEN MODULE CALLS:          M32. 247
      DATA   L  / 0 /                                                   M32. 248
C     BEGINNING OF ASSIGNMENT OF RANDOM NUMBERS TO ARRAY  'A' .         M27. 403
      A(  1) =  .1445312506618                                          M27. 404
      A(  2) =  .8477795260409                                          M27. 405
      A(  3) =  .8267723125831                                          M27. 406
      A(  4) =  .6660710406131                                          M27. 407
      A(  5) =  .7152322826372                                          M27. 408
      A(  6) =  .3239128029543                                          M27. 409
      A(  7) =  .5051959208554                                          M27. 410
      A(  8) =  .3805491872180                                          M27. 411
      A(  9) =  .5609474043286                                          M27. 412
      A( 10) =  .5996361115942                                          M27. 413
      A( 11) =  .4159594349328                                          M27. 414
      A( 12) =  .6756609755246                                          M27. 415
      A( 13) =  .0995378032610                                          M27. 416
      A( 14) =  .6033780421273                                          M27. 417
      A( 15) =  .4515533431030                                          M27. 418
      A( 16) =  .0020932062778                                          M27. 419
      A( 17) =  .9161858062074                                          M27. 420
      A( 18) =  .3434229008090                                          M27. 421
      A( 19) =  .7876940045781                                          M27. 422
      A( 20) =  .2760908032985                                          M27. 423
      A( 21) =  .3665660303205                                          M27. 424
      A( 22) =  .8204300122029                                          M27. 425
      A( 23) =  .2413831265551                                          M27. 426
      A( 24) =  .1246653115746                                          M27. 427
      A( 25) =  .2802441882439                                          M27. 428
      A( 26) =  .0466535013838                                          M27. 429
      A( 27) =  .4742772736449                                          M27. 430
      A( 28) =  .9477027545777                                          M27. 431
      A( 29) =  .2260144770748                                          M27. 432
      A( 30) =  .2987460629005                                          M27. 433
      A( 31) =  .5203589181526                                          M27. 434
      A( 32) =  .8981967037721                                          M27. 435
      A( 33) =  .3873885562436                                          M27. 436
      A( 34) =  .5780913804991                                          M27. 437
      A( 35) =  .1280852320759                                          M27. 438
      A( 36) =  .3327754064471                                          M27. 439
      A( 37) =  .4043867414456                                          M27. 440
      A( 38) =  .9490362532099                                          M27. 441
      A( 39) =  .6261391859471                                          M27. 442
      A( 40) =  .3291406705415                                          M27. 443
      A( 41) =  .3366864607083                                          M27. 444
      A( 42) =  .9438413593777                                          M27. 445
      A( 43) =  .9369008057740                                          M27. 446
      A( 44) =  .0713971670442                                          M27. 447
      A( 45) =  .6500854844946                                          M27. 448
      A( 46) =  .9737952005663                                          M27. 449
      A( 47) =  .6485758973471                                          M27. 450
      A( 48) =  .7724301318424                                          M27. 451
      A( 49) =  .9676692044394                                          M27. 452
      A( 50) =  .5163953619955                                          M27. 453
      A( 51) =  .5788464270596                                          M27. 454
      A( 52) =  .7758933795560                                          M27. 455
      A( 53) =  .0910635448877                                          M27. 456
      A( 54) =  .0439510552688                                          M27. 457
      A( 55) =  .0707223001462                                          M27. 458
      A( 56) =  .9379043319315                                          M27. 459
      A( 57) =  .0052391978463                                          M27. 460
      A( 58) =  .9420572226295                                          M27. 461
      A( 59) =  .5932597508799                                          M27. 462
      A( 60) =  .6466146627873                                          M27. 463
      A( 61) =  .4395400252824                                          M27. 464
      A( 62) =  .1972895298303                                          M27. 465
      A( 63) =  .5017482047726                                          M27. 466
      A( 64) =  .1503404202877                                          M27. 467
      A( 65) =  .9624699228977                                          M27. 468
      A( 66) =  .0098276069324                                          M27. 469
      A( 67) =  .6571365402082                                          M27. 470
      A( 68) =  .4233003554891                                          M27. 471
      A( 69) =  .1203194365765                                          M27. 472
      A( 70) =  .7436871629477                                          M27. 473
      A( 71) =  .8136524161969                                          M27. 474
      A( 72) =  .7311319136405                                          M27. 475
      A( 73) =  .0594772166524                                          M27. 476
      A( 74) =  .2374863512189                                          M27. 477
      A( 75) =  .2450459940689                                          M27. 478
      A( 76) =  .4326371816340                                          M27. 479
      A( 77) =  .3562832359564                                          M27. 480
      A( 78) =  .3723442477773                                          M27. 481
      A( 79) =  .1694432139356                                          M27. 482
      A( 80) =  .3735622812899                                          M27. 483
      A( 81) =  .0610718353086                                          M27. 484
      A( 82) =  .2782657746530                                          M27. 485
      A( 83) =  .5137050416289                                          M27. 486
      A( 84) =  .4340395038268                                          M27. 487
      A( 85) =  .5766543446808                                          M27. 488
      A( 86) =  .4413641042052                                          M27. 489
      A( 87) =  .9812390285872                                          M27. 490
      A( 88) =  .2625281459037                                          M27. 491
      A( 89) =  .9554345097074                                          M27. 492
      A( 90) =  .4741647690234                                          M27. 493
      A( 91) =  .9906793757886                                          M27. 494
      A( 92) =  .7820837820369                                          M27. 495
      A( 93) =  .2206664815389                                          M27. 496
      A( 94) =  .0901816247992                                          M27. 497
      A( 95) =  .7625227133400                                          M27. 498
      A( 96) =  .4434728419824                                          M27. 499
      A( 97) =  .7905859532294                                          M27. 500
      A( 98) =  .9796097207935                                          M27. 501
      A( 99) =  .7599602497147                                          M27. 502
      A(100) =  .1154361048406                                          M27. 503
C     END       OF ASSIGNMENT OF RANDOM NUMBERS TO ARRAY  'A' .         M27. 504
      IF ( X  .EQ.  0.0 )   GO TO 2614                                  M27. 505
      L = (KNT - 1 ) * KSWTCH + 1                                       M32. 249
      GO TO 2632                                                        M27. 507
 2614 L = L + 1                                                         M27. 508
 2632 N1 = (L-1) / 100                                                  M27. 509
      IF ( IPRSUP  .GE.  1 )                                            M27. 510
     1WRITE (LUNIT6, 2645)  L, KNT, KSWTCH, N1, X                       M27. 511
 2645 FORMAT ( /, 114H VARIABLES IN  'SANDNM' ,   THE RANDOM-NUMBER GENEM27. 512
     1RATOR WITH 100 BUILT-IN NUMBERS.       L     KNT  KSWTCH      N1 ,M27. 513
     2 14X, 1HX  ,/, 82X, 4I8, E15.5 )                                  M27. 514
      IF ( N1  .GT.  0 )                                                M27. 515
     1 L = L  -  100 * N1                                               M27. 516
      SANDNM = A(L)                                                     M27. 517
      RETURN                                                            M27. 518
      END                                                               M27. 519
      SUBROUTINE  MULT(A,X,Y,N,ICHECK)                                  M42. 783
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    SUBROUTINE  'MULT'  IS USED TO POST-MULTIPLY A SYMMETRIC MATRIX   M15. 333
C)    BY A VECTOR.                                                      M15. 334
C)    A=MATRIX,X AND Y=VECTORS.IF ICHECK=0   THEN  Y=A*X                M15. 335
C)    N=SIZE OF MATRIX.        IF ICHECK=NEG.THEN  Y=A*X-Y              M15. 336
C)                             IF ICHECK=POS.THEN  Y=A*X+Y.             M15. 337
C)    MATRIX A IS REAL, SYMMETRIC AND STORED AS UPPER TRIANGULAR MATRIX M15. 338
C)    IN ONE-DIMENSIONAL ARRAY (1 ELEMENT FOR FIRST COLUMN, 2 FOR SECONDM15. 339
C)    COLUMN ETC.). Y MUST NOT BE IDENTICAL WITH X.                     M15. 340
      DIMENSION A(1), X(1), Y(1)                                        M42. 784
      II=0                                                              M42. 785
      K=0                                                               M42. 786
    1 K=K+1                                                             M42. 787
      IF(K.GT.N) RETURN                                                 M42. 788
      XX=X(K)                                                           M42. 789
      YY=Y(K)                                                           M42. 790
      IF(ICHECK.EQ.0) YY=0.                                             M42. 791
      IF(ICHECK.LT.0) YY=-YY                                            M42. 792
      DO 2 I=1,K                                                        M42. 793
      II=II+1                                                           M42. 794
      Y(I)=Y(I)+A(II)*XX                                                M42. 795
    2 YY=YY+A(II)*X(I)                                                  M42. 796
      Y(K)=YY                                                           M42. 797
      GO TO 1                                                           M42. 798
      END                                                               M42. 799
