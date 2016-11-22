      SUBROUTINE OVER51
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      IF ( IPRSUP  .GE.  1 )                                            M32,2229
     1 WRITE ( LUNIT6, 4567 )  KILL                                     M39.3624
 4567 FORMAT ( 24H BEGIN "OVER51".  KILL =,  I8 )                       M39.3625
      CALL RUNTYM ( VMIN, VMAX )                                           16382
      FLSTAT(9) = VMIN - FLSTAT(9)                                         16383
      FLSTAT(10) = VMAX - FLSTAT(10)                                       16384
      IF ( KILL .EQ. 0 )  GO TO 4521                                            
      CALL SUBR51                                                               
      GO TO 4536                                                                
 4521 LASTOV = NCHAIN                                                   M39.3629
      NCHAIN = 55                                                       M39.3630
 4536 IF ( IPRSUP  .GE.  1 )                                            M39.3631
     1 WRITE ( LUNIT6, 4568 )                                           M32,2233
 4568 FORMAT ( 22H EXIT MODULE "OVER51". )                              M39.3632
99999 RETURN                                                            M39.3633
      END                                                               M39.3634
      SUBROUTINE SUBR51                                                 M39.3635
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      DIMENSION  KPEN(20)                                               M39.3637
C     NOTE. --- AS THE STRUCTURE OF THE EMTP ERROR OVERLAYS CHANGE,     M39.3638
C               THE FOLLOWING ASSIGNMENTS MAY HAVE TO BE ALTERED ...    M39.3639
C                    NFRFLD  ---- TOTAL NUMBER OF ERROR OVERLAYS        M39.3640
C                    KPEN(J) ---- STORAGE FOR THE HIGHEST KILL-CODE     M39.3641
C                                 NUMBER HANDLED BY ERROR OVERLAY       M39.3642
C                                 NUMBER  50+J ,   FOR ALL BUT THE      M39.3643
C                                 LAST ERROR OVERLAY.                   M39.3644
      DATA  KPEN(1)   /  50   /                                         M39.3645
      DATA  KPEN(2)   /  90   /                                         M39.3646
      DATA  KPEN(3)   /  150  /                                         M39.3647
      DATA  KPEN(4)   /  200  /                                         M39.3648
      NFRFLD = 5                                                        M39.3649
      IF ( IPRSUP  .GE.  1 )                                            M39.3650
     1 WRITE ( LUNIT6, 4567 )  KILL                                     M39.3651
 4567 FORMAT ( 24H BEGIN "SUBR51".  KILL =,  I8 )                       M39.3652
      IF ( KILL .EQ. 99  .AND.  LSTAT(19) .EQ. 11111 )                  M39.3653
     1 CALL STOPTP                                                              
      KPEN(NFRFLD) = INTINF                                             M39.3655
      CALL RUNTYM ( VMIN, VMAX )                                        M39.3656
      FLSTAT(9) = VMIN - FLSTAT(9)                                      M39.3657
      IF ( IPNTV(1)  .NE.  -8888 )   GO TO 1643                         M39.3658
C     FOLLOWING ASSIGNMENTS ARE TO DEFINE EXCEPTIONAL /BLANK/           M32,2235
C     VARIABLES USED IN  "KILL CODES"  USAGE:                           M32,2236
      IF ( KILL  .EQ.  38 )   FLSTAT(12) = 1.0                          M32,2237
      IF (KILL  .EQ. 41)   LSTAT(10) = 1                                M32,2238
      IF (KILL  .EQ. 43)  FLSTAT(12) = 1.                               M32,2239
      IF ( KILL - 1  .NE.  IPNTV(3) )   GO TO 1643                      M22.7241
      IPNTV(3) = KILL                                                   M22.7242
      GO TO 1649                                                        M22.7243
 1643 IF ( LASTOV  .GE.  NCHAIN )   GO TO 1649                          M22.7244
      NOUTPR = 0                                                        M21. 553
      CALL INTERP                                                               
      WRITE(LUNIT6, 5315)                                               M17.1479
 5315 FORMAT( 1H  )                                                        16387
      WRITE(LUNIT6, 5316)                                               M17.1480
 5316 FORMAT( 132H -----------------------------------------------------   16389
     1------------------------------------------------------------------   16390
     2------------     )                                                   16391
      DO 5319  I=1, 2                                                      16392
 5319 WRITE(LUNIT6, 5320)                                               M17.1481
 5320 FORMAT( 132H ERROR/ERROR/ERROR/ERROR/ERROR/ERROR/ERROR/ERROR/ERROR   16394
     1/ERROR/ERROR/ERROR/ERROR/ERROR/ERROR/ERROR/ERROR/ERROR/ERROR/ERROR   16395
     2/ERROR/ERROR     )                                                   16396
      WRITE(LUNIT6, 5316)                                               M17.1482
      IF ( M4PLOT .EQ. 1 )  GO TO 5534                                          
      WRITE(LUNIT6, 5332)                                               M17.1483
 5332 FORMAT(118H0YOU LOSE, FELLA.   THE EMTP LOGIC HAS DETECTED AN ERROM25.4652
     1R CONDITION, AND IS GOING TO TERMINATE YOUR RUN.  THE FOLLOWING  ,   16400
     2 /, 121H PRINTOUT MESSAGE SUMMARIZES THE DATA DIFFICULTY LEADING T   16401
     3O THIS PROGRAM DECISION.  BY STUDYING THIS MESSAGE, THE PROBLEM  ,   16402
     4 /,118H DATA,  AND THE RULES DELINEATED BY THE  840-PAGE EMTP RULEM37.9129
     5 BOOK,  IT IS HOPED THAT THE USER CAN RECTIFY THE PROBLEM.     ,  M37.9130
     6 /,  81H IF STILL IN DOUBT AFTER SOME STUDY, COME SEE PROGRAM MAIN   16405
     7TENANCE FOR ASSISTANCE.    )                                         16406
      DECODE (80, 1019, ABUFF(1) )   ( TEXCOL(J), J=1, 14 )             M22.7246
 1019 FORMAT ( 13A6, A2 )                                               M22.7247
      WRITE (LUNIT6, 5333)  ( TEXCOL(J), J=1, 14 )                      M22.7248
 5333 FORMAT( 113H WHERE AN OTHERWISE-UNIDENTIFIED CARD IS REFERRED TO,    16408
     1OR IS CALLED THE 'LAST' CARD, THIS MEANS THE MOST-RECENTLY-   ,/,    16409
     2113H READ CARD OF THE INPUT DATA DECK.   THE 80-COLUMN CARD IMAGE    16410
     3IN QUESTION IS THE LAST ONE PRINTED OUT BEFORE THIS   ,/,            16411
     4 42H TERMINATION MESSAGE.   A COPY FOLLOWS....,  13A6, A2 )       M22.7249
 5534 WRITE (LUNIT6, 5335)  KILL, LSTAT(18), LSTAT(19)                  M37.9131
 5335 FORMAT( 14X, 16HKILL CODE NUMBER, 16X, 14HOVERLAY NUMBER,            16414
     1 10X, 20HNEARBY STATEMENT NO.  , /, 3I30 )                           16415
 1649 DO 1658  J=1, NFRFLD                                              M22.7250
      IF ( KILL  .GT.  KPEN(J) )   GO TO 1658                           M22.7251
      IF ( J  .EQ.  1 )   GO TO 1684                                    M22.7252
      LASTOV = NCHAIN                                                   M22.7253
      NCHAIN = 50 + J                                                   M22.7254
      IF ( IPRSUP  .GE.  1 )                                            M32,2240
     1 WRITE ( LUNIT6, 1652 )   KILL, J, KPEN(J), NCHAIN                M32,2241
 1652 FORMAT ( 21H ERROR OVERLAY FOUND.,                                M32,2242
     1         29H   KILL, J, KPEN(J), NCHAIN =,  4I8  )                M32,2243
      GO TO 9000                                                        M39.3659
 1658 CONTINUE                                                          M22.7256
 1684 CONTINUE                                                          M22.7257
      GO TO ( 6001, 6002, 6003, 6004, 6005, 6006, 6007, 6008, 6009,        16417
     1 6010, 6011, 6012, 6013, 6014, 6015, 6016, 6017, 6018, 6019, 6020,   16418
     2 6021, 6022, 6023, 6024, 6025, 6026, 6027, 6028, 6029, 6030,         16419
     3 6031, 6032, 6033, 6034, 6035, 6036, 6037, 6038, 6039, 6040,         16420
     4 6041, 6042, 6043, 6044, 6045, 6046, 6047, 6048, 6049, 6050), KILL   16421
 6001 WRITE(LUNIT6, 7001)  LSTAT(16)                                    M17.1486
 7001 FORMAT( 5X, 33HSTORAGE EXCEEDED FOR LIST NUMBER , I2, 46H.   SEE D   16423
     1IMENSIONED LIMIT IN TABULATION BELOW.  ,/,                           16424
     2 5X, 112HTHE PROBLEM BEING INPUTTED IS SIMPLY TOO BIG FOR THE PROG   16425
     3RAM AS CURRENTLY DIMENSIONED.   SINCE THERE USUALLY ARE ,/,          16426
     4 5X, 110HWAYS OF CIRCUMVENTING THIS DIFFICULTY, IT IS SUGGESTED TH   16427
     5AT THE USER CONSULT HIS FRIENDLY NEIGHBORHOOD PROGRAM   ,/,          16428
     6 5X,  16HMAINTENANCE MAN.   )                                        16429
      GO TO 6220                                                           16430
 6002 IF ( T  .LT.  0.0 )   GO TO 7102                                  M20.6998
      WRITE (LUNIT6, 7002)  FLSTAT(16)                                  M20.6999
 7002 FORMAT( 5X, 108HTIME-STEP SIZE 'DELTAT' AS READ FROM COLUMNS 1-8 O   16432
     1F THE FIRST MISCELLANEOUS DATA CARD IS NOT POSITIVE.   THE  ,/,      16433
     2 5X,  23HUSER PUNCHED A VALUE OF , E14.4,  75H.   UNLIKE JULES VER   16434
     3NE, YOU ARE NOT ALLOWED TO STOP OR DECREASE TIME DURING  ,/,         16435
     4 5X, 111HA SIMULATION, MY FRIEND.   DON'T TRY RIDING OUT OF AN ACT   16436
     5IVE VOLCANO ON A RAFT FLOATING ON MOLTEN LAVA, EITHER,   ,/,         16437
     6 5X,  30HALTHOUGH THAT'S ANOTHER STORY.   )                          16438
      GO TO 6220                                                           16439
 7102 WRITE (LUNIT6, 7202)  T                                           M20.7000
 7202 FORMAT (5X, 103HTHE STARTING TIME   'T'   AS READ FROM THE FLOATINM20.7001
     1G-POINT MISCELLANEOUS DATA CARD IS NEGATIVE, WHICH IS        ,/,  M20.7002
     2 5X,  21HILLEGAL.   A VALUE OF,    E15.5,     49H   WAS READ FROM M20.7003
     3THE DATA FIELD OF COLUMNS 49-56.      )                           M20.7004
      GO TO 6220                                                        M20.7005
 6003 WRITE(LUNIT6, 7003)  LSTAT(16)                                    M17.1488
 7003 FORMAT( 5X,  91HILLEGAL TYPE CODE READ FROM COLUMNS 1-2 OF LAST BR   16441
     1ANCH CARD.   THE USER PUNCHED A VALUE OF , I2, 10H.   EITHER ,/,     16442
     2 5X, 113HTHE PUNCHED NUMBER ITSELF IS PATENTLY ILLEGAL (ALWAYS AN    16443
     3ERROR, UNDER ANY CIRCUMSTANCES), OR THE CARD IN QUESTION  ,/,        16444
     4 5X, 107HIS OUT OF SEQUENCE IN RELATION TO THE PRECEDING DATA CARD   16445
     5 WHICH WAS INPUTTED.   AS AN EXAMPLE OF THE LATTER  ,/,              16446
     6 5X, 113HCASE, CONSIDER A '-3' PUNCH, WITH THE PRECEEDING CARD NOT   16447
     7 BEARING A '-2' PUNCH.   IN ANY CASE, OPEN UP THE USER'S  ,/,        16448
     8 5X,  93HMANUAL, AND REREAD THE RULES FOR THE DATA TYPE THAT YOU H   16449
     9AVE BEEN TRYING TO INPUT, MY FRIEND.    )                            16450
      WRITE (LUNIT6, 7103)                                              M17.1489
 7103 FORMAT (5X, 109HYET, IN CASE THIS GENERAL ADVICE DOES NOT SEEM TO M12. 685
     1APPLY, CONSIDER THE POSSIBLE TROUBLE WHICH CAN ARISE FROM A   ,/, M12. 686
     2 5X, 113HPRECEDING FAULTY USE OF THE REFERENCE-BRANCH CAPABILITY. M12. 687
     3  THIS FEATURE HAS THE EMTP LOOKING FOR A CERTAIN NUMBER   ,/,    M12. 688
     4 5X, 112HAND TYPE OF BRANCH CARDS, BASED ON PROPERTIES OF THE COMPM12. 689
     5ONENT TO WHICH REFERENCE HAS BEEN MADE.   IF THE USER'S   ,/,     M12. 690
     6 5X, 113HDATA CARDS DO NOT IN STRUCTURE MATCH THOSE OF THE REFERENM12. 691
     7CE COMPONENT, AN ERROR STOP OF THE PRESENT TYPE MAY VERY    )     M12. 692
      WRITE (LUNIT6, 7203)                                              M17.1490
 7203 FORMAT (5X, 109HBE EXPECTED.   REMEMBER THAT IN CASES WHERE TWO ORM12. 694
     1 MORE BRANCHES FROM REFERENCE BUS  'BUS3'  TO REFERENCE BUS   ,/, M12. 695
     2 5X, 110H'BUS4'  (ORDERED PAIR OF NAMES, READ FROM COLUMNS 15-24 AM12. 696
     3S  2A6  INFORMATION) EXIST, THE EMTP WILL ALWAYS PICK    ,/,      M12. 697
     4 5X, 78 HTHE FIRST ONE THAT IT FINDS (IN ORDER OF BRANCH INPUT) FOM12. 698
     5R REFERENCE PURPOSES.     )                                       M12. 699
      GO TO 6220                                                           16451
 6004 WRITE(LUNIT6, 7004)                                               M17.1491
 7004 FORMAT( 5X, 108HLAST BRANCH WAS ZERO IMPEDANCE (R, L, C FIELDS OF    16453
     1COLUMNS 27-44 WERE ALL ZERO).   IF YOU REALLY WANT A SHORT   ,/,     16454
     2   5X, 111HCIRCUIT, YOU MUST PUNCH A VERY SMALL VALUE FOR  R  OR     16455
     3L.   OR BETTER YET, WHY NOT DO AWAY WITH ONE OF THE NODE   ,/,       16456
     4 5X,  83HNAMES OF THIS BRANCH, TREATING BOTH ENDS AS THE SAME BUS    16457
     5(A PERFECT SHORT CIRCUIT).   )                                       16458
      GO TO 6220                                                           16459
 6005 WRITE(LUNIT6, 7105)                                               M17.1492
 7105 FORMAT( 5X, 114HTHE USER HAS BEEN INPUTTING PAIRS OF POINTS WHICH    16461
     1DEFINE A NONLINEAR (OR PSEUDO-NONLINEAR) ELEMENT CHARACTERISTIC. ,   16462
     2 /, 5X, 108HTHESE MUST BE IN ORDER, MOVING EVER TO THE RIGHT AND U   16463
     3PWARD IN THE X-Y PLANE.   BUT THE USER'S JUST-INPUTTED     )         16464
      WRITE(LUNIT6, 7005)                                               M17.1493
 7005 FORMAT( 5X, 107HCHARACTERISTIC IS NOT MONOTONE INCREASING AS IT SH   16466
     1OULD BE.   A DECREASE IN ONE OF THE TWO COORDINATE POINTS   ,/,      16467
     2 5X,  85HHAS BEEN DETECTED ON THE LAST DATA CARD WHICH WAS READ.     16468
     3 SHAPE UP OR SHIP OUT, JACK.   )                                     16469
      GO TO 6220                                                           16470
 6006 WRITE(LUNIT6, 7006)  BUS3, BUS4                                   M17.1494
 7006 FORMAT( 5X, 114HREFERENCE BRANCH NAMES OF LAST DATA CARD DO NOT RE   16472
     1FER TO PREVIOUSLY-INPUTTED BRANCH OF SAME TYPE.   THE DATA BEING ,   16473
     2 /, 5X, 116HREFERENCED CAN NOT BE FOUND.   CHECK FOR SPELLING ERRO   16474
     3RS, OR POSITION DIFFERENCES OF BLANKS WITHIN THE FIELD WIDTHS. ,/,   16475
     4 5X,  58HDEFECTIVE NAMES AS READ OFF CARD FROM COLUMNS 15-26 WERE    16476
     5', A6, 7H' AND ', A6, 2H'.   )                                       16477
      GO TO 6220                                                           16478
 6007 WRITE(LUNIT6, 7007)  LSTAT(16)                                    M17.1495
 7007 FORMAT( 5X, 106HAN ILLEGAL TYPE CODE HAS BEEN READ FROM COLUMNS 1-   16480
     12 OF THE LAST DATA CARD, WHICH THE EMTP BELIEVES TO BE A ,/,      M25.4653
     2 5X,  40HSWITCH CARD.   THE EMTP READ A VALUE OF , I2,  46H.   BUTM25.4654
     3 SUCH A VALUE CANNOT BE ACCEPTED, SINCE   ,/,                        16483
     4  5X, 43HONLY TYPE CODES  0 (ORDINARY SWITCH),  92 (,             M28.9140
     5      48HSWITCHED RESISTANCE),  93 (SWITCHED INDUCTANCE),,        M28.9141
     6 /5X, 45H  11 (DIODE AND TACS-CONTROLLED VALVE),  12 (,           M28.9142
     7      35HTACS-CONTROLLED SPARK GAP OR TRIAC),                     M28.9143
     8 /5X, 46HAND  13 (TACS-CONTROLLED ORDINARY SWITCH)  ARE,          M28.9144
     9      10H ALLOWED.     )                                          M28.9145
      GO TO 6220                                                           16487
 6008 WRITE(LUNIT6, 7008)                                               M17.1496
 7008 FORMAT( 5X, 111HAN ERROR IN THE ORDERING OF SWITCH CARDS HAS OCCUR   16489
     1ED.   ALL SWITCHED-RESISTANCE ELEMENTS (TYPE-92) MUST PRECEDE ,/,    16490
     2   5X, 104HANY SWITCHED-INDUCTANCE (TYPE-93) ELEMENTS.   THE LAST    16491
     3DATA CARD WHICH HAS BEEN READ VIOLATES THIS RULE.    ,/,             16492
     4 5X,  27HGET WITH IT, GUY, SHAPE UP.   )                             16493
      GO TO 6220                                                           16494
 6009 WRITE(LUNIT6, 7109)                                               M17.1497
 7109 FORMAT( 5X,  97HTHE LAST DATA CARD WHICH WAS READ HAS BEEN TAKEN B   16496
     1Y THE EMTP TO BE A SWITCHED-INDUCTANCE ELEMENT.    )              M25.4655
      WRITE(LUNIT6, 7009)  FLSTAT(16)                                   M17.1498
 7009 FORMAT( 5X, 108HBUT THIS COMPONENT  HAS SATURATION (BREAKPOINT) FL   16499
     1UX NONPOSITIVE, WHICH IS ABSURD.   FROM COLUMNS 45-54, THE   ,/,     16500
     2 5X,  22HPROGRAM READ THE VALUE , E12.3, 25H FROM THE LAST DATA CA   16501
     3RD.  )                                                               16502
      GO TO 6220                                                           16503
 6010 WRITE(LUNIT6, 7010)  LSTAT(16)                                    M17.1499
 7010 FORMAT( 5X, 105HTHE LAST DATA CARD WHICH WAS READ HAS BEEN TAKEN T   16505
     1O BE A SOURCE CARD.   BUT IT BEARS AN ILLEGAL TYPE CODE  ,/,         16506
     2 5X,  27HIN COLUMNS 1-2 (A VALUE OF , I2,  79H WAS READ).   OPEN U   16507
     3P THE USER'S MANUAL, AND REREAD THE RULES FOR SOURCE CARDS.  )       16508
      GO TO 6220                                                           16509
 6011 WRITE(LUNIT6, 7011)  LSTAT(16)                                    M17.1500
 7011 FORMAT( 5X,  93HTYPE-16 CONTROLLED DC VOLTAGE SOURCE CARD HAS ILLE   16511
     1GAL INITIAL CONDITION CODE IN COLUMNS 9-10.      ,/,                 16512
     2 5X,  19HTHE VALUE READ WAS , I2 )                                   16513
      GO TO 6220                                                           16514
 6012 WRITE(LUNIT6, 7012)  BUS1                                         M17.1501
 7012 FORMAT( 5X, 104HTHE BUS NAME READ FROM COLUMNS 3-8 OF THE LAST SOU   16516
     1RCE CARD IS UNRECOGNIZABLE, NOT HAVING BEEN PREVIOUSLY  ,/,          16517
     2 5X,  77HDEFINED BY A BRANCH CARD OR A SWITCH CARD.   THE NAME REA   16518
     3D FROM THE CARD IS ', A6,  20H'.   THE USER IS NOT  ,/,              16519
     4 5X, 112HALLOWED TO CONNECT SOURCES TO NETWORK NODES WHICH ARE NOT   16520
     5 A PART OF THE NETWORK PREVIOUSLY DEFINED BY BRANCH AND ,/,          16521
     6 5X, 115HSWITCH CARDS OF THIS DATA CASE.   AFTER ALL, SUCH NODES W   16522
     7OULD BE COMPLETELY DISCONNECTED FROM THE NETWORK, SO COULD  ,/,      16523
     8 5X, 116HNOT AFFECT THE NETWORK SOLUTION.   MOST PROBABLY, ONE OR    16524
     9MORE SPELLING ERRORS HAVE BEEN MADE IN PUNCHING NODE NAMES.  )       16525
      WRITE(LUNIT6, 7131)                                               M17.1502
      GO TO 6220                                                           16527
 6013 WRITE(LUNIT6, 7013)                                               M17.1503
 7013 FORMAT( 5X,  34H SIMULATOR VOLTAGE OUTSIDE LIMITS.   )               16529
      GO TO 6220                                                           16530
 6014 WRITE (LUNIT6, 7014)  BUS1, BUS2, FLSTAT(16)                      M17.1504
 7014 FORMAT( 5X,  35HTYPE-93 NONLINEAR INDUCTANCE FROM ', A6,             16532
     1 6H' TO ', A6,  37H' HAS PSI-STEADY (COLUMNS 33-38) ZERO   ,/,       16533
     2 5X,  71HWHILE I-STEADY (COLUMNS 27-32) IS NONZERO.   VALUE READ F   16534
     3OR I-STEADY = , E12.3, 2H . , /,                                     16535
     4 5X,  47HSUCH ZERO-IMPEDANCE BRANCHES ARE NOT PERMITTED.   )         16536
      GO TO 6220                                                           16537
 6015 WRITE(LUNIT6, 7015)  LSTAT(15), LSTAT(16)                         M17.1505
 7015 FORMAT( 5X,  86HBREAKDOWN OF LOGIC IN STEADY-STATE SETUP BEFORE CA   16539
     1LL TO SUBROUTINE NUMBER.   NEXT, II=, 2I10 )                         16540
      WRITE(LUNIT6, 7115)                                               M17.1506
 7115 FORMAT( 5X, 110HTHIS IS PROBABLY A PROGRAM BUG, NOT THE FAULT OF T   16542
     1HE USER.   GO SEEK HELP FROM PROGRAM MAINTENANCE ABOUT THIS.   )     16543
      GO TO 6220                                                           16544
 6016 WRITE(LUNIT6, 7016)  LSTAT(16), NTOT, LSTAT(15)                   M17.1507
 7016 FORMAT( 5X,104HARRAYS 'KOLUM' AND 'KORDER' OF THE RENUMBERING CALC   16546
     1ULATION FOR THE STEADY-STATE NETWORK HAVE OVERFLOWED.   ,/,          16547
     2 5X, 115HTHE PROBLEM IS SIMPLY TOO BIG OR TOO DENSE (ADMITTANCE MA   16548
     3TRIX HAS TOO MANY NONZERO ELEMENTS) FOR THE EXISTING ARRAY  ,/,      16549
     4 5X,   8HSIZES OF,  I7,  18H   CELLS.   OF THE,  I6,  64H   MATRIXM19.3424
     5 ROWS TOTAL, OVERFLOW HAS OCCURRED WHILE WORKING ON THE      ,/,  M19.3425
     6 5X, 29HSIMULATED ELIMINATION FOR ROW, I4,  64H.   THIS OVERFLOW T   16552
     7ROUBLE IS REALLY TROUBLE WITH DEPENDENT LIST         )            M10. 375
      WRITE (LUNIT6, 7116)                                              M17.1508
 7116 FORMAT (5X,  56HNUMBER 99, ABOUT WHICH AN EXPLANATION WILL FOLLOW    16555
     1BELOW.     ,/,                                                       16556
     2 5X,  87HIF THE CORE STORAGE NEEDED TO PREVENT THIS OVERFLOW CAN N   16557
     3OT BE SPARED, AS A LAST RESORT     ,/,                               16558
     8 5X, 112HTHE USER MIGHT BYPASS THE STEADY-STATE CALCULATION COMPLE   16559
     9TELY, AND SIMPLY RUN IN THE TRANSIENTS MODE FOR SEVERAL  )           16560
      WRITE(LUNIT6, 7216)                                               M17.1509
 7216 FORMAT( 5X, 104HCYCLES, HOPING THAT BY THEN THE SOLUTION WILL HAVE   16562
     1 SETTLED SUFFICIENTLY INTO THE SINUSOIDAL STEADY-STATE  ,/,          16563
     2 5X,  61H(FOLLOWING THE ENERGIZATION SHOCK WHICH OCCURS AT TIME ZE   16564
     3RO).    )                                                            16565
      KILL = 1                                                             16566
      LSTAT(16) = 99                                                       16567
      NCHAIN = 51                                                       M22.7258
      IF ( IPRSUP  .GE.  1 )                                            M32,2244
     1 WRITE ( LUNIT6, 8943 )  NCHAIN, KILL                             M39.3660
      GO TO 9000                                                        M39.3661
 6017 WRITE(LUNIT6, 7017)                                               M17.1510
 7017 FORMAT( 5X, 111HLOGIC BREAKDOWN IN STEADY-STATE SOLUTION AFTER REN   16570
     1UMBERING OF THE S.S. NETWORK HAS BEEN SUCCESSFULLY COMPLETED.  )     16571
      WRITE(LUNIT6, 7115)                                               M17.1511
      WRITE (LUNIT6, 7117)                                              M25.4656
 7117 FORMAT ( 5X,  31HBUT THEN AGAIN, MAYBE IT IS NOT,                 M25.4657
     1       38H THE FAULT OF THE EMTP.  SEVERAL YEARS,                 M37.9132
     2       37H OF EXPERIENCE WITH THIS MESSAGE HAVE   ,/,             M25.4659
     3         5X,  30HSHOWN THAT THE USER'S DATA WAS,                  M25.4660
     4       41H INVARIABLY ERRONEOUS, THOUGH NOT STOPPED,              M37.9133
     5       40H AT THE TIME THE BRANCHES WERE INPUTTED.   )            M25.4662
      WRITE (LUNIT6, 7217)  BUS1, LSTAT(15)                             M25.4663
 7217 FORMAT ( 5X,  29HFOR EXAMPLE, THERE MIGHT BE A,                   M25.4664
     1       40H BRANCH BOTH ENDS OF WHICH TOUCH NODE  ',               M25.4665
     2         A6,  25H' ,   FOUND IN ROW NUMBER,   I4,/,2X,            M32,2246
     3       40H   OF THE BRANCH TABLE (ORDER OF INPUT).  )             M25.4667
      GO TO 6220                                                           16573
 6018 WRITE (LUNIT6, 7018)  LSTAT(14), BUS1, BUS2                       M20.7006
 7018 FORMAT( 5X, 109HIN FORMING THE NETWORK ADMITTANCE MATRIX FOR THE S   16575
     1TEADY-STATE PHASOR SOLUTION FOR INITIAL CONDITIONS, TROUBLE ,/,      16576
     2 5X,  34HHAS OCCURRED WHILE PROCESSING THE , I2,                     16577
     3 55H-PHASE COUPLED R-L ELEMENTS THE FIRST OF WHICH CONNECTS, /,5X,   16578
     4  5HBUS ', A6, 12H' WITH BUS ', A6,  81H'.   THIS COULD EITHER BE    16579
     5AN ACTUAL COUPLED R-L GROUP, OR THE SERIES SUB-BRANCHES ,/,          16580
     6 5X, 115HOF A MULTI-PHASE PI-CIRCUIT.   IN EITHER CASE, THE COUPLE   16581
     7D-BRANCH-GROUP IMPEDANCE MATRIX    (Z) = (R) + JW(L)   HAS ,/,       16582
     8 5X, 112HBEEN FOUND TO BE NEAR-SINGULAR (UNINVERTIBLE).   IN PARTI   16583
     9CULAR, A NEAR-ZERO DIAGONAL ELEMENT HAS BEEN FOUND JUST  )           16584
      WRITE(LUNIT6, 7118)  FLSTAT(11), FLSTAT(12), FLSTAT(13)           M17.1513
 7118 FORMAT( 5X, 112HBEFORE RECIPROCATION, DURING THE INVERSION ATTEMPT   16586
     1.   USING MAGNITUDES SQUARED FOR ALL THREE QUANTITIES, WE HAVE ,/,   16587
     2 5X, 25HORIGINAL DIAGONAL VALUE =, E12.3,                            16588
     3 24H,   QUESTIONABLE VALUE =, E12.3, 21H,   TOLERANCE RATIO =,       16589
     4 E12.3, 1H. ,/,  5X,                                                 16590
     5 117HTHE USER HAD BETTER CHECK HIS DATA VALUES FOR THIS COUPLED GR   16591
     6OUP VERY CAREFULLY, TO SEE IF SOMETHING IS NOT ABNORMAL.   )         16592
      WRITE(LUNIT6, 7218)                                               M17.1514
 7218 FORMAT( 5X, 104HTHE PROGRAM TOLERANCE RATIO FOR NEAR-ZERO CHECKING   16594
     1 IS MISCELLANEOUS DATA PARAMETER 'TOLMAT'.   PERHAPS A   ,/,         16595
     2 5X, 109HREREADING OF THE USER'S MANUAL EXPLANATION OF THIS INPUT    16596
     3CONSTANT WOULD BE IN ORDER, SHOULD THE USER STILL BE   ,/,           16597
     4 5X,  40HUNCERTAIN OF THE REASON FOR HIS TROUBLE.   )                16598
      GO TO 6220                                                           16599
 6019 WRITE(LUNIT6, 7019)                                               M17.1515
 7019 FORMAT( 5X,  90HSTEADY-STATE SOLUTION LOGIC TROUBLE.   SEE PROGRAM   16601
     1 MAINTENANCE, SINCE THIS IS THEIR FAULT.   )                         16602
      GO TO 6220                                                           16603
 6020 WRITE(LUNIT6, 7020)                                               M17.1516
 7020 FORMAT( 5X,  74HLOGIC BREAKDOWN IN STEADY-STATE SOLUTION WHILE BUI   16605
     1LDING ADMITTANCE MATRIX.   )                                         16606
      WRITE(LUNIT6, 7115)                                               M17.1517
      GO TO 6220                                                           16608
 6021 WRITE(LUNIT6, 7021)                                               M17.1518
 7021 FORMAT( 5X, 105HSTEADY-STATE SOLUTION BREAKDOWN BECAUSE OVER 25 NO   16610
     1DES OF NETWORK ARE SWITCHED TOGETHER.   THIS REPRESENTS  ,/,         16611
     2 5X, 113HOVERFLOW OF ARRAY ITEMP(??).   TO SOLVE THE PROBLEM, SIMPM31.8293
     3LY REDIMENSION  LIST 26  , OR GET RID OF THAT INORDINATE  , /,    M31.8294
     4 5X, 118HNUMBER OF SWITCHES WHICH ARE ALL CONNECTED TOGETHER.   TH   16614
     5AT ANY LEGITIMATE POWER-SYSTEM PROBLEM SHOULD EVER REACH THIS  ,/,   16615
     6 5X, 109HTERMINATION IS HIGHLY IMPROBABLE, AND SOMEWHAT SUSPECT.     16616
     7 UNLESS THE USER UNDERSTANDS WHAT IS WRONG, AND WHY,   ,/,           16617
     8 5X, 104HSOME CONSULTATION WITH THE USER'S FRIENDLY NEIGHBORHOOD P   16618
     9ROGRAM MAINTENANCE MAN IS STRONGLY RECOMMENDED.   )                  16619
      GO TO 6220                                                           16620
 6022 WRITE(LUNIT6, 7022)  LSTAT(16)                                    M17.1519
 7022 FORMAT( 5X,  70HBIG TROUBLE.   SINGULAR NODAL ADMITTANCE MATRIX IN   16622
     1VOLVING NODE NUMBER , I3,                                            16623
     1                    41H OF STEADY-STATE SOLUTION HAS BEEN FOUND. ,   16624
     2 /, 5X, 103HBUT USER BUS NUMBER FOR THIS NODE COULD NOT BE FOUND I   16625
     3N RENUMBERING MAP  NORDER.   PROGRAM LOGIC ERROR.    )               16626
      WRITE(LUNIT6, 7115)                                               M17.1520
      GO TO 6220                                                           16628
 6023 WRITE(LUNIT6, 7023)  BUS1                                         M17.1521
 7023 FORMAT( 5X, 105HNO.   WE CAN NOT VALIDLY SET VOLTAGE OF DISCONNECT   16630
     1ED SUBNETWORK LAST IDENTIFIED TO ZERO, SINCE IT APPEARS   , /,       16631
     2 5X, 111HTHAT A CURRENT SOURCE FEEDS THIS SUBNETWORK.   FOR THIS C   16632
     3ASE, EITHER KIRCHHOFF'S CURRENT LAW FOR THE SUBNETWORK  ,/,          16633
     4 5X,    114HIS VIOLATED (CURRENT IN DOES NOT EQUAL CURRENT OUT), O   16634
     5R THE SOLUTION IS INDETERMINATE (SUBNETWORK VOLTAGE SOLUTION  ,/,    16635
     6 5X, 107HIS ONLY DETERMINED UP TO AN ARBITRARY CONSTANT).   IN EIT   16636
     7HER CASE, THE PROBLEM IS ILL-POSED PHYSICALLY, AND  ,/,              16637
     8 5X,  84HMUST BE REJECTED.   USER HAD BETTER RECONSIDER HIS NETWOR   16638
     9K IN THE VICINITY OF NODE ', A6,  19H', TO EITHER REMOVE    )        16639
      WRITE(LUNIT6, 7123)                                               M17.1522
 7123 FORMAT( 5X,  51HTHE SINGULARITY, OR THE CURRENT SOURCE(S), OR BOTH   16641
     1.  )                                                                 16642
      GO TO 6220                                                           16643
 6024 WRITE(LUNIT6, 7024)  LSTAT(16), LSTAT(15), LSTAT(14)              M17.1523
 7024 FORMAT( 5X, 68HLOGIC FAILURE NEAR END STEADY-STATE SOLUTION.   KK,   16645
     1 IA, INDEX(IA+1)=, 3I10 )                                            16646
      WRITE(LUNIT6, 7115)                                               M17.1524
      GO TO 6220                                                           16648
 6025 WRITE(LUNIT6, 7025)  ( LSTAT(I), I=11, 16 )                       M17.1525
 7025 FORMAT( 5X,  79HLOGIC TROUBLE WITHIN STEADY-STATE RENUMBERING.   N   16650
     1EXT, MEXT, IB, I, JBS, NELIM=, 6I8 )                                 16651
      WRITE(LUNIT6, 7115)                                               M17.1526
      GO TO 6220                                                           16653
 6026 WRITE(LUNIT6, 7026)                                               M17.1527
 7026 FORMAT( 5X,  77HLOGIC ERROR IN STEADY-STATE RENUMBERING.   KOLUM A   16655
     1RRAY HAS ERRONEOUS NUMBERS.   )                                      16656
      WRITE(LUNIT6, 7115)                                               M17.1528
      GO TO 6220                                                           16658
 6027 WRITE(LUNIT6, 7127)  BUS1, BUS2, BUS3                             M32,2247
 7127 FORMAT ( 5X, 28HSOURCE BEING READ IS TYPE-18,                     M32,2248
     1             31H IDEAL-TRANSFORMER PLUS SOURCE.,                  M32,2249
     2             32H   BUT ONE OR MORE OF THE COLUMN   ,/,            M32,2250
     3         5X, 34H3-22 NAME FIELDS ARE UNRECOGNIZED.,               M32,2251
     4             17H   NAMES ARE ....,    3( 2X, 1H", A6,             M32,2252
     5                      1H"),  2H .                  ,/,            M32,2253
     6         5X, 31HUNLESS ALL THREE ARE LEGAL EMTP,                  M32,2254
     7             30H NETWORK NODE NAMES,  THE CASE,                   M32,2255
     8             30H CAN NOT BE RUN.   CORRECT IT.     )              M32,2256
      GO TO 6220                                                           16677
 6028 WRITE (LUNIT6, 7028)                                              M36.3639
 7028 FORMAT ( 5X, 32HTHE LAST-READ COMPONENT HAS BEEN,                 M36.3640
     1             34H IDENTIFIED AS AN OLD-FORMAT (PRE-,               M36.3641
     2             34H"M37.") ZINC-OXIDE SURGE ARRESTER.  ,/,           M36.3642
     3         5X, 34HSUCH OLD DATA MUST BE CONVERTED TO,               M36.3643
     4             35H THE NEWER FORMAT.   THE SUPPORTING,              M36.3644
     5             32H PROGRAM "ZINOLD" (SEE RULE BOOK    ,/,           M36.3645
     6         5X, 39HINDEX) WILL PERFORM ALL SUCH CONVERSION,          M36.3646
     7             33H FOR THE USER AUTOMATICALLY, IN A,                M36.3647
     8             22H SINGLE PASS.  TRY IT.               )            M36.3648
      GO TO 6220                                                           16684
 6029 WRITE (LUNIT6, 7029)  BUS1, BUS2, FLSTAT(16)                      M17.1534
 7029 FORMAT( 5X,  51HDISTRIBUTED-PARAMETER BRANCH CARD CONNECTING NODE    16686
     1', A6, 6H' TO ', A6,  29H' REPRESENTS PROPAGATION MODE , /,          16687
     2 5X,  24HWITH TRAVEL TIME (SEC) =, E14.5,  79H.   THIS IS LESS THA   16688
     3N TIME-STEP SIZE  DELTAT, A CASE WHICH IS ILLEGAL.   EITHER , /,     16689
     4 5X,  94HRUN WITH SMALL ENOUGH DELTAT, OR REPLACE THE DISTRIBUTED    16690
     5MODELLING BY LUMPED PI-CIRCUIT MODEL.   )                            16691
      IF( FLSTAT(16)  .EQ.  0.0 )                                          16692
     1 WRITE(LUNIT6, 7129)                                              M17.1535
 7129 FORMAT( 5X, 104HTHE JUST-PRINTED TRAVEL TIME IS SEEN TO BE ZERO, T   16694
     1HOUGH, WHICH IS GENERALLY A DEFAULT VALUE DESERVING OF    ,/,        16695
     2 5X, 112HFURTHER COMMENT.   ASSUMING THAT THE USER HAS ACTUALLY IN   16696
     3PUTTED A LINE OF FINITE LENGTH, THE TRAVEL TIME MUST OF  ,/,         16697
     4 5X, 110HCOURSE BE POSITIVE.   A ZERO VALUE IS USED FOR DEFAULT IN   16698
     5 THE CASE OF A FREQUENCY-DEPENDENT MODE, HOWEVER, FOR   ,/,          16699
     6 5X, 110HTHE CASE WHERE THE INFINITE-FREQUENCY TRAVEL TIME IS LESS   16700
     7 THAN THE TIME-STEP-SIZE  'DELTAT'  THAT THE USER HAS   ,/,          16701
     8 5X, 115HSELECTED.   TO FIND THE ACTUAL OFFENDING TRAVEL-TIME VALU   16702
     9E, THE USER CAN CONSULT THE DATA-CARD INTERPRETATION WHICH   )       16703
      IF( FLSTAT(16) .EQ. 0.0 )                                            16704
     1 WRITE(LUNIT6, 7229)                                              M17.1536
 7229 FORMAT( 5X, 108HGOES ALONG WITH THE ASSOCIATED BRANCH CARD AS IT W   16706
     1AS READ IN.   THE EMTP DOES NOT HAVE THE VALUE IN QUESTION  ,/,   M25.4668
     2 5X, 112HIMMEDIATELY AVAILABLE AT THIS POINT, AND CAN ONLY TELL TH   16708
     3E USER THAT HE'S IN TROUBLE (WITHOUT KNOWING HOW MUCH).   )          16709
      GO TO 6220                                                           16710
 6030 WRITE(LUNIT6, 7030)  LSTAT(16)                                    M17.1537
 7030 FORMAT( 5X,  47HFROM COLUMNS 1-2 OF LAST DATA CARD, A VALUE OF ,     16712
     1 I2,  52H WAS READ.   THIS IS ILLEGAL.   THE CARD IN QUESTION , /,   16713
     2 5X, 111HSHOULD BE EITHER AN INITIAL-CONDITION CARD, WHERE VALUES    16714
     32, 3, OR 4 ARE LEGAL, OR IT IS AN OUTPUT-SPECIFICATION  , /,         16715
     4 5X,  57HCARD, WHERE VALUES 0 OR 1 ARE ALLOWED.   SHAPE UP, FELLA.   16716
     5 )                                                                   16717
      GO TO 6220                                                           16718
 6031 WRITE(LUNIT6, 7031)  BUS1                                         M17.1538
 7031 FORMAT( 5X,  84HLAST CARD GIVES NODE-VOLTAGE INITIAL CONDITION FOR   16720
     1 UNRECOGNIZABLE NODE.   THE NAME ', A6,  11H' WHICH WAS ,/,          16721
     2 5X,  52HPUNCHED COULD NOT BE FOUND IN THE LIST OF BUS NAMES.  )     16722
      WRITE(LUNIT6, 7131)                                               M17.1539
 7131 FORMAT( 5X, 105HCHECK NAME(S) FOR SPELLING, AS WELL AS THE COLUMN-   16724
     1POSITIONING OF ANY BLANKS IN THE FIELD(S) WHICH IS(ARE)  ,/,         16725
     2 5X,  20HSIX CHARACTERS WIDE.   )                                    16726
      GO TO 6220                                                           16727
 6032 WRITE (LUNIT6, 7032)  BUS1, BUS2                                  M17.1540
 7032 FORMAT( 5X,  71HLAST CARD GIVES INITIAL-CONDITON CURRENTS FOR LINE   16729
     1AR BRANCH FROM NODE ', A6, 6H' TO ', A6, 1H. )                       16730
 7232 WRITE(LUNIT6, 7132)                                               M17.1541
 7132 FORMAT( 5X, 112HBUT THE BRANCH REFERRED TO COULD NOT BE FOUND IN T   16732
     1HE APPROPRIATE LIST OF BRANCHES.   IS THE ORIENTATION CORRECT.  )    16733
      WRITE(LUNIT6, 7131)                                               M17.1542
      GO TO 6220                                                           16735
 6033 WRITE (LUNIT6, 7033)  BUS1, BUS2                                  M17.1543
 7033 FORMAT( 5X,  75HLAST CARD GIVES INITIAL CONDITION CURRENTS FOR NON   16737
     1LINEAR BRANCH FROM NODE ', A6, 6H' TO ', A6, 1H.  )                  16738
      GO TO 7232                                                           16739
 6034 WRITE(LUNIT6, 7034)  LSTAT(13), LSTAT(14), FLSTAT(15), FLSTAT(16) M17.1544
 7034 FORMAT( 5X,  80HLAST CARD GIVES INITIAL CONDITION WHICH IS ILLEGAL   16741
     1 FOR NONLINEAR ELEMENT NUMBER , I3, 18H.   THE LAST POINT ,/,        16742
     2 5X,  60HOF THE NONLINEAR CHARACTERISTIC IS STORED IN TABLE LOCATI   16743
     3ON , I3, 15H AND HAS VALUE , E13.4, 15H.   BUT INITIAL ,/,           16744
     4 5X,  30HCONDITION REQUIRES A VALUE OF , E13.4,  68H, WHICH IS OFF   16745
     5 THE END OF THE CHARACTERISTIC.   USER MUST EXTEND THE ,/,           16746
     6 5X,  70HCHARACTERISTIC, IF THIS IS REALLY THE INITIAL CONDITION T   16747
     7HAT HE WANTS.   )                                                    16748
      GO TO 6220                                                           16749
 6035 WRITE(LUNIT6, 7035)  LSTAT(16), BUS1, BUS2,                       M17.1545
     1 FLSTAT(11), FLSTAT(12), FLSTAT(13)                                  16751
 7035 FORMAT( 5X,  90HBREAKDOWN OF SOLUTION WHEN USING COMPENSATION (SUP   16752
     1ERPOSITION) ON NONLINEAR ELEMENT NUMBER , I3,  /,                    16753
     2 5X,  21HWHICH CONNECTS NODE ', A6, 6H' TO ', A6, 2H'.  ,/,          16754
     3 5X, 114HTHEVENIN LOAD LINE DOES NOT INTERSECT THE NONLINEAR CHARA   16755
     4CTERISTIC.   MAYBE USER SHOULD EXTEND THE CHARACTERISTIC, /,         16756
     5 5X, 112HASSUMING THAT THE PROBLEM IS OTHERWISE WELL-POSED.   SOME   16757
     6 PROGRAM SOLUTION QUANTITIES BEARING ON THIS BREAKDOWN,   ,/,        16758
     7 5X,  91HPOTENTIALLY USEFUL TO YOUR PROGRAM MAINTENANCE MAN (IF NO   16759
     8T TO THE USER), ARE AS FOLLOWS ...   ,/,                             16760
     9 5X,  16HE(K), E(M), A2 = , 3E13.4  )                                16761
      WRITE (LUNIT6, 6135)                                              M17.1546
 6135 FORMAT (5X, 108HPAST EXPERIENCE BASED ON A NUMBER OF ENCOUNTERS WIM14.4367
     1TH THIS EMTP ERROR STOP HAS SHOWN THAT MOST OFTEN THE USER    ,/, M14.4368
     2 5X, 110HHAS UNDER-ESTIMATED THE PEAK NONLINEAR (OR TIME-VARYING) M14.4369
     3ELEMENT ORDINATE VALUE.   THAT IS, THE CHARACTERISTIC        ,/,  M14.4370
     4 5X, 116HAS INPUTTED WAS CORRECT, PERHAPS, BUT WAS SIMPLY NOT DEFIM14.4371
     5NED FOR LARGE ENOUGH VALUES.   REMEMBER THAT TRUE NONLINEAR   ,/, M14.4372
     6 5X, 103HOR TIME-VARYING ELEMENTS (WITH TYPE CODES 91, 92, OR 93) M14.4373
     7DO NOT HAVE THE FINAL SEGMENTS ON THE TWO ENDS                )   M14.4374
      WRITE (LUNIT6, 6235)                                              M17.1547
 6235 FORMAT (5X, 111HEXTRAPOLATED LINEARLY TO INFINITY (AS IS THE CASE M14.4376
     1WITH PSEUDO-NONLINEAR ELEMENTS OF TYPE CODES 97, 98, AND 99).  ,/,M14.4377
     2 5X, 105HBEFORE LOOKING FOR ANY MORE SOPHISTICATED TROUBLE, THIS VM14.4378
     3ERY SIMPLE POSSIBILITY SHOULD ALWAYS BE CHECKED.            )     M14.4379
      GO TO 6220                                                           16762
 6036 WRITE (LUNIT6, 7036)  LSTAT(16), BUS1, BUS2,                      M17.1548
     1 FLSTAT(12), FLSTAT(13), FLSTAT(11)                                  16764
 7036 FORMAT( 5X,  63HBREAKDOWN SURROUNDING NONLINEAR OR TIME-VARYING EL   16765
     1EMENT NUMBER , I3, 24H HAVING TERMINAL NODES ', A6,                  16766
     2 7H' AND ', A6, 2H'.  ,/,                                            16767
     3 5X,  99HTHIS IS A TIME-VARYING RESISTANCE WITH LAST TIME POINT OF   16768
     4 CHARACTERISTIC DEFINED BY USER BEING FOR , E13.4,  /,               16769
     5 5X, 11HSEC.   BUT , E13.4,   89H SEC HAVE ELAPSED SINCE ELEMENT W   16770
     6AS TURNED ON.   USER MUST EXTEND CHARACTERISTIC IF STUDY  ,/,        16771
     7 5X,  66HIS TO BE RUN OUT FURTHER THAN THE PRESENT SIMULATION TIME   16772
     8 OF   T =, E12.3,  31H SEC   AT WHICH THIS RUN IS NOW  ,/,           16773
     9 5X,  17HBEING TERMINATED.   )                                       16774
      GO TO 6220                                                           16775
 6037 WRITE (LUNIT6, 7037)                                              M32,2259
 7037 FORMAT  (5X, 32HLAST DATA CARD WAS DECODED USING,                 M32,2260
     1             41H  4E16.0  AND CAME UP WITH FLOATING POINT,        M32,2261
     2             35H ZEROS.   THIS IS NOT LEGAL FOR THE               M32,2262
     3   ,/,   5X, 34HMODAL IMPEDANCE DATA OF FREQUENCY-,               M32,2263
     4             34HDEPENDENT GENERATOR EQUIVALENTS AS,               M32,2264
     5             29H WERE BEING READ IN "FDDATA".   )                 M32,2265
      GO TO 6220                                                           16786
 6038 D1 = FLSTAT(13) / FLSTAT(12)                                         16787
      WRITE(LUNIT6, 7238)                                               M17.1552
 7238 FORMAT( 5X,  34HMATRIX (Y) IS THE NODAL ADMITTANCE,               M37.9134
     1             18H MATRIX WHICH MUST  ,/,                           M37.9135
     2 5X, 101HBE RE-TRIANGULARIZED AT EVERY TIME-STEP THAT A SWITCH OR    16791
     3PSEUDO-NONLINEAR ELEMENT CHANGES ITS STATUS.    )                    16792
      WRITE (LUNIT6, 7038)  LSTAT(16), BUS1, FLSTAT(14), D1, FLSTAT(12) M17.1553
 7038 FORMAT( 5X,  41HTHERE HAS JUST BEEN A BREAKDOWN FACTORING,        M37.9136
     1  30H (Y), WHILE ELIMINATING ON ROW, I4,                          M37.9137
     3  29H  WHICH CORRESPONDS TO NODE ", A6, 10H".   AFTER  ,/,        M37.9138
     4 5X,  92HELIMINATION TO THE LEFT OF THE DIAGONAL IN THIS ROW, THE    16798
     5DIAGONAL ELEMENT WAS FOUND TO EQUAL, E13.4,   7H, WHILE   ,/,        16799
     6 5X,  59HTHE ORIGINAL VALUE BEFORE ANY ELIMINATION WAS PERFORMED W   16800
     7AS, E13.4,  34H.   THE RATIO OF THESE TWO NUMBERS   ,/,              16801
     8 5X,  34HEXCEEDS THE NEAR-ZERO TOLERANCE OF, E13.4,  49H, WHICH IS   16802
     9 MISCELLANEOUS DATA PARAMETER 'EPSILN'.   )                          16803
      WRITE(LUNIT6, 7338)                                               M17.1554
 7338 FORMAT( 5X, 114HTHE PROGRAM THINKS THAT EITHER THE NODE IN QUESTIO   16805
     1N, OR A SUBNETWORK TO WHICH IT BELONGS, IS DISCONNECTED FROM THE )   16806
      WRITE(LUNIT6, 7138)                                               M17.1555
 7138 FORMAT(  5X,                                                         16808
     1     116HREST OF THE NETWORK.   IF ACTUALLY DISCONNECTED, USER SHO   16809
     2ULD ADD A CONNECTING BRANCH.   IF CONNECTED BY A VERY-HIGH-  ,/,     16810
     3 5X,  99HIMPEDANCE BRANCH, EITHER MAKE  EPSILN  NEARER TO ZERO, OR   16811
     4 DECREASE THE IMPEDANCE OF THE CONNECTION.    ,/,                    16812
     5 5X, 109HBUT BE CAREFUL IN RESORTING TO A SMALLER 'EPSILN', AS NUM   16813
     6ERICAL SOLUTION DIFFICULTY WILL INEVITABLY RESULT AS   ,/,           16814
     7 5X, 115H'EPSILN' APPROACHES THE COMPUTER ROUNDOFF LIMIT (DETERMIN   16815
     8ED BY COMPUTER WORD-SIZE).   THE USER IS ADVISED TO REREAD   )       16816
      WRITE(LUNIT6, 7438)                                               M17.1556
 7438 FORMAT( 5X, 110HTHE EMTP USER'S MANUAL EXPLANATION OF THE SIGNIFICM25.4670
     1ANCE OF 'EPSILN', SHOULD HE BE IN DOUBT.   A CALL TO PROGRAM ,/,     16819
     2 5X,  49HMAINTENANCE MIGHT ALSO BE IN ORDER, IN THIS CASE.  )        16820
      WRITE (LUNIT6, 7538)                                              M17.1557
 7538 FORMAT (/, 5X, 104HIT MIGHT BE DESIRABLE TO SAY A FEW WORDS ABOUT M13.3868
     1HOW THE PRINTED 6-CHARACTER NODE NAME, AND THE SUBNETWORK     ,/, M13.3869
     2 5X, 116HIN QUESTION, ARE RELATED.   HAVING AN ISOLATED SUBNETWORKM13.3870
     3 MEANS THAT THE SYSTEM MATRIX WHICH IS BEING TRIANGULARIZED    ,/,M13.3871
     4 5X, 110HIS BLOCK DIAGONAL, WITH THE PROBLEM SUBNETWORK THEN ASSOCM13.3872
     5IATED WITH A SINGULAR (UNINVERTIBLE) BLOCK.   THIS IS       ,/,   M13.3873
     6 5X, 114HDECOUPLED FROM THE REST OF THE SYSTEM, AND MATHEMATICALLYM13.3874
     7 THE ASSOCIATED EQUATIONS HAVE AN INDETERMINATE SOLUTION.     )   M13.3875
      WRITE (LUNIT6, 7638)                                              M17.1558
 7638 FORMAT (5X, 106HTHIS SHOWS UP AS A ZERO DIAGONAL ELEMENT WHILE ELIM13.3877
     1MINATING ON THE ROW FOR THE LAST NODE OF THE SUBNETWORK.      ,/, M13.3878
     2 5X, 113HWHICH NODE WILL BE THE LAST OF THE SUBNETWORK IS A FUNCTIM13.3879
     3ON OF INTERNAL EMTP RENUMBERING, AS WELL AS THE ORDER OF     ,/,  M13.3880
     4 5X, 111HTHE NETWORK INPUT DATA.   HENCE THE USER SHOULD NOT ASSUMM13.3881
     5E THAT THERE IS TROUBLE SPECIFICALLY AT THE NODE WHOSE      ,/,   M13.3882
     6 5X, 111HNAME IS PRINTED ABOVE.   USE OF NETWORK CONNECTIVITY PRINM13.3883
     7TOUT WILL ALLOW RECONSTRUCTION OF THE PROBLEM TOPOLOGY     )      M13.3884
      WRITE (LUNIT6, 7738)                                              M17.1559
 7738 FORMAT (5X, 109HIN THE VICINITY OF THE PRINTED NODE.   THE USER ISM13.3886
     1 ADVISED TO GO AS FAR AWAY FROM THIS NODE IN ALL DIRECTIONS    ,/,M13.3887
     2 5X, 115HAS IS NECESSARY, UNTIL THE LIMITS TO THE SUBNETWORK IN QUM13.3888
     3ESTION BECOME CLEAR.   A KNOWLEDGE OF SWITCH STATUS AT THE     ,/,M13.3889
     4 5X, 110HINSTANT OF DIFFICULTY IS OF COURSE CRUCIAL.   OFTEN THIS M13.3890
     5ERROR STOP WILL OCCUR JUST AS A SWITCH OPENS, THEREBY    ,/,      M13.3891
     6 5X, 111HSEGMENTING THE SYSTEM INTO TWO PIECES, ONE OF WHICH HAS NM13.3892
     7O PATHS TO GROUND (WHICH THEN BECOMES THE DISCONNECTED       ,/,  M13.3893
     8 5X,  11HSUBSYSTEM).          )                                   M13.3894
      GO TO 6220                                                           16821
 6039 WRITE (LUNIT6, 7039)  LSTAT(16), BUS1, BUS2                       M17.1560
 7039 FORMAT( 5X,  63HSOLUTION BREAKDOWN ON NONLINEAR OR TIME-VARYING EL   16823
     1EMENT NUMBER , I3, 18H CONNECTING NODE ', A6, 13H' WITH NODE ',      16824
     2 A6, 2H'.  ,/,                                                       16825
     3 5X, 114HONE OR MORE OTHER SUCH ELEMENTS IS ACTUALLY TOUCHING THIS   16826
     4 ONE, RATHER THAN BEING SEPARATED FROM IT BY THE REQUIRED  ,/,       16827
     5 5X,  12HTRAVEL TIME.   )                                            16828
      WRITE(LUNIT6, 7139)                                               M17.1561
 7139 FORMAT( 5X, 107HWHILE THE PROBLEM MAY BE WELL-POSED PHYSICALLY, TH   16830
     1E PRESENT EMTP SOLUTION ASSUMPTIONS CONCERNING THE USE OF  ,/,    M25.4671
     2 5X, 110HCOMPENSATION CANNOT HANDLE SUCH A CASE.   TO REMEDY THIS    16832
     3DILEMA, THE USER HAS TWO GENERAL POTENTIAL COURSES OF    ,/,         16833
     4 5X,  11HACTION ....   ,/,                                           16834
     5 8X, 106H1) ELIMINATE THE OFFENDING NONLINEAR OR TIME-VARYING ELEM   16835
     6ENT(S).   IF THE OFFENDING ELEMENTS ARE NONLINEAR   ,/,              16836
     7 11X, 102HRATHER THAN TIME-VARYING, USE MIGHT BE MADE OF SWITCHED    16837
     8OR PSEUDO-NONLINEAR ELEMENTS OF THE SAME TYPE.     )              M37.9139
      WRITE(LUNIT6, 7239)                                               M17.1562
 7239 FORMAT( 11X,  99HWHILE THIS CONVERTS TRUE NONLINEARITIES TO PSEUDO   16840
     1-NONLINEARITIES WHICH INHERENTLY LAG ONE TIME STEP   ,/,             16841
     2 11X, 112HBEHIND THE SOLUTION (AND THUS MUST BE USED WITH CAUTION    16842
     3TO ENSURE THAT MOVEMENT ALONG THE DESIRED CHARACTERISTIC  ,/,        16843
     4 11X,  77HIS SLOW), ONE DOES AVOID ALL ISOLATION PROBLEMS ASSOCIAT   16844
     5ED WITH COMPENSATION.    ,/,                                         16845
     6 8X, 115H2) INTRODUCE SUFFICIENT STUB LINES (SMALL SERIES-INDUCTAN   16846
     7CE ELEMENTS WHICH ARE REPRESENTED BY LOSSLESS DISTRIBUTED-  ,/,      16847
     8 11X, 109HPARAMETER LINES HAVING TRAVEL-TIME EQUAL TO THE TIME-STE   16848
     9P-SIZE 'DELTAT') TO ISOLATE THE INTERFERING ELEMENTS.   )            16849
      WRITE(LUNIT6, 7339)                                               M17.1563
 7339 FORMAT( 11X, 104HIF 'DELTAT' IS SMALL ENOUGH, SUCH ADDED ELEMENTS    16851
     1MAY NOT SERIOUSLY DISTURB THE PROBLEM PHYSICALLY, WHILE  ,/,         16852
     2 11X,  31HPROVIDING THE NEEDED ISOLATION.   ,/,                      16853
     3 5X, 111HAS BOTH OF THESE REMEDIES ARE POTENTIALLY DANGEROUS IF NO   16854
     4T USED WITH CONSIDERABLE CAUTION AND UNDERSTANDING, IT   ,/,         16855
     5 5X, 115HIS SUGGESTED THAT THE USER APPLY SOME THOUGHT TO THE MATT   16856
     6ER WHILE STUDYING THE USER'S MANUAL ABOUT THESE QUESTIONS.  ,/,      16857
     7 5X,  98HA CALL TO PROGRAM MAINTENANCE FOR SOME PROFESSIONAL ADVIC   16858
     8E MIGHT ALSO BE APPROPRIATE, IF IN DOUBT.    )                       16859
      GO TO 6220                                                           16860
 6040 WRITE(LUNIT6, 7040)  LSTAT(15), LSTAT(16), BUS5, BUS6, BUS3, BUS4 M17.1564
 7040 FORMAT( 5X,  90HSOLUTION BREAKDOWN USING COMPENSATION METHOD ON NO   16862
     1NLINEAR OR TIME-VARYING ELEMENTS NUMBER , I3, 5H AND , I3, 1H. ,/,   16863
     2 5X,  25HTHE FIRST CONNECTS NODE ', A6, 13H' WITH NODE ', A6,        16864
     3  40H', WHILE THE SECOND HAS TERMINAL NODES ', A6, 7H' AND ', A6,    16865
     4 2H'.  ,/,                                                           16866
     5 5X,  94HTHESE TWO ELEMENTS ARE NOT SEPARATED BY THE REQUIRED TRAV   16867
     6EL TIME (DISTRIBUTED-PARAMETER LINE).    )                           16868
      WRITE(LUNIT6, 7139)                                               M17.1565
      WRITE(LUNIT6, 7239)                                               M17.1566
      WRITE(LUNIT6, 7339)                                               M17.1567
      GO TO 6220                                                           16872
 6041 WRITE(LUNIT6, 7041)  LSTAT(10), BUS1, BUS2                        M17.1568
 7041 FORMAT( 5X, 14HSWITCH NUMBER , I3, 18H CONNECTING NODE ', A6,        16874
     1 13H' WITH NODE ', A6,  30H' HAS CLOSED, CAUSING TROUBLE.  )         16875
      GO TO 6040                                                           16876
 6042 WRITE(LUNIT6, 7142)                                               M17.1569
 7142 FORMAT( 5X, 103HMATRIX  (YAA)  IS THE FIRST-PARTITION DIAGONAL SUB   16878
     1MATRIX AMONG NON-SWITCH (OR PSEUDO-NONLINEAR-ELEMENT)   ,/,          16879
     2 5X,  83HNODES WHICH IS TO BE TRIANGULARIZED ONCE AND FOR ALL OUTS   16880
     3IDE OF THE TIME-STEP LOOP.    )                                      16881
      WRITE(LUNIT6, 7042)  LSTAT(16), BUS1                              M17.1570
 7042 FORMAT( 5X, 4HROW , I3, 102H IN YAA (MATRIX FOR NONSWITCH ROWS) HA   16883
     1S IDENTICALLY-ZERO DIAGONAL ELEMENT BEFORE STARTING ELIMINATION. ,   16884
     2 /, 5X,  26HTHIS CORRESPONDS TO NODE ', A6,  79H', WHERE THERE IS    16885
     3EITHER NOTHING CONNECTED, OR THERE IS A VERY STRANGE, PERFECT  ,/,   16886
     4 5X, 117HRESONANCE.   THE USER SHOULD CHECK ALL BRANCHES CONNECTED   16887
     5 TO THE NODE IN QUESTION.   IF THERE ARE NONE, PUT ONE IN OR  ,/,    16888
     6 5X,  26HDROP THE NODE IN QUESTION.   )                              16889
      GO TO 6220                                                           16890
 6043 WRITE(LUNIT6, 7142)                                               M17.1571
      D1 = FLSTAT(13) / FLSTAT(12)                                         16892
      WRITE(LUNIT6, 7043)  LSTAT(16), BUS1, FLSTAT(14), D1, FLSTAT(12)  M17.1572
 7043 FORMAT( 5X,  81HTHERE HAS BEEN A BREAKDOWN IN THE TRIANGULARIZATIO   16894
     1N OF (YAA) WHILE WORKING ON ROW, I4,  19H, WHICH CORRESPONDS  ,/,    16895
     2 5X,   9HTO NODE ', A6, 93H'.   AFTER ELIMINATION TO THE LEFT OF T   16896
     3HE DIAGONAL IN THIS ROW, THE DIAGONAL ELEMENT HAS BEEN  ,/,          16897
     4 5X,  14HFOUND TO EQUAL, E13.4,  62H, WHILE THE ORIGINAL MATRIX VA   16898
     5LUE (BEFORE ANY ELIMINATION) WAS, E13.4,   7H.   THE  ,/,            16899
     6 5X,  61HRATIO OF THESE TWO NUMBERS EXCEEDS THE NEAR-ZERO TOLERANC   16900
     7E OF, E13.4,  41H, WHICH IS MISC. DATA PARAMETER 'EPSILN'.   )       16901
      WRITE(LUNIT6, 7338)                                               M17.1573
      WRITE(LUNIT6, 7138)                                               M17.1574
      WRITE(LUNIT6, 7438)                                               M17.1575
      GO TO 6220                                                           16905
 6044 WRITE(LUNIT6, 7044)                                               M17.1576
 7044 FORMAT( 5X, 104HYOU DUMMY, YOU FORGOT TO REQUEST OUTPUT VARIABLES.   16907
     1   IF THE BRANCH CARDS HAVE NO COLUMN-80 BRANCH-OUTPUT  ,/,          16908
     2 5X, 115HREQUESTS, AND NO NODE VOLTAGES WERE REQUESTED FOR OUTPUT,   16909
     3 THERE IS NOTHING TO LOOK AT BY WAY OF A PROBLEM SOLUTION.  ,/,      16910
     4 5X, 108HNO VARIABLES ARE TO BE PRINTED, AND NONE CAN BE PLOTTED.    16911
     5  THE EMTP WILL NOT LET YOU RUN A PROBLEM THIS WAY.   ,/,         M25.4672
     6 5X,  88HGO BACK TO THE KEYPUNCH AND ADD SOME OUTPUT, DO NOT PASS    16913
     7GO, DO NOT COLLECT 200 DOLLARS.    )                                 16914
      GO TO 6220                                                           16915
 6045 WRITE (LUNIT6, 7145)  LSTAT(17),                                  M29.6151
     1                      LSTAT(14), FLSTAT(15), FLSTAT(16)           M29.6152
 7145 FORMAT (5X,  20HWEIGHTING FUNCTION A,  I1,                        M29.6153
     1             35H(T) JUST INPUTTED DOES NOT HAVE ALL,              M29.6154
     2             31H WHICH ARE MONOTONE INCREASING.       ,/,         M29.6155
     2 5X,  28HIN PARTICULAR, POINT NUMBER , I3,  12H IS FOR TIME,         16921
     3 E15.6,  39H SEC, WHILE THE PRECEDING POINT WAS FOR    ,/,           16922
     4 5X,   4HTIME, E15.6,  91H.   PERHAPS THE CARD DECK HAS BEEN SHUFF   16923
     5LED, OR SOMEHOW ALTERED FROM THAT AS PUNCHED BY THE ,/,              16924
     6 5X,  38HWEIGHTING-FUNCTION SUPPORTING PROGRAM.   )                  16925
      GO TO 6220                                                           16926
 6046 WRITE( LUNIT6, 7046 ) LSTAT(15), FLSTAT(15), LSTAT(14)            M29.6156
 7046 FORMAT( 5X,  60HTHE WEIGHTING FUNCTION FREQUENCY DEPENDENT LINE MOM29.6157
     1DE NUMBER ,I2,  11H HAS FAILED ,/, 5X,  61HDURING AN ATTEMPT TO IMM29.6158
     2PROVE THE LOW FREQUENCY REPRESENTATION ,/, 5X,  64HBY ADJUSTING THM29.6159
     3E EXPONENTIAL TIME CONSTANT OF THE RESPONSE TAILS ,/, 5X,  70HTO GM29.6160
     4IVE THE THEORETICALLY CORRECT AREAS.  THE ADJUSTMENT WAS REQUESTEDM29.6161
     5 ,/, 5X,  21HBY A NON-ZERO VALUE = ,E10.5,  33H FOR "RTOTAL" IN COM29.6162
     6LUMNS 65 TO 72 ,/, 5X,  76HOF THE MISCELLANEOUS DATA CARD, WHILE AM29.6163
     7 PERMISSIBLE ITERATION COUNT OF NII = ,I4, /, 5X,  65HWAS PROVIDEDM29.6164
     8 FOR THE NEWTON-RAPHSON ALGORITHM IN COLUMNS 73 TO 80 )           M29.6165
      GO TO 6220                                                        M29.6166
 6047 WRITE(LUNIT6, 7047)  LSTAT(16)                                    M17.1580
 7047 FORMAT( 5X, 102HSWITCH-ARRAY OVERFLOW DUE TO TOO MANY SATURABLE TR   16931
     1ANSFORMERS.   THE SWITCH TABLES ARE USED FOR SCRATCH  ,/,            16932
     2 5X, 108HSTORAGE OF XFORMER STARTING-AND-ENDING DATA POINTERS, DUR   16933
     3ING BRANCH-DATA INPUT.   THE USER HAS INPUTTED MORE  ,/,             16934
     4 5X,  65HSATURABLE XFORMERS THAN THE PROGRAM LIMIT FOR SWITCHES, W   16935
     5HICH IS , I3,  41H.   EITHER REDUCE THE FORMER, OR INCREASE ,/,      16936
     6 5X,  11HTHE LATTER.   )                                             16937
      GO TO 6220                                                           16938
 6048 WRITE(LUNIT6, 7048 )  BUS3                                        M17.1581
 7048 FORMAT( 5X, 104HTHE LAST DATA CARD INPUTTED REQUESTS A COPY OF A P   16940
     1RECEDING SATURABLE TRANSFORMER WHICH PURPORTEDLY BEARS ,/,           16941
     2 5X,  31HINTERNAL IDENTIFYING BUS NAME ', A6,  80H'.   BUT A SEARC   16942
     3H OF PRECEDING COMPONENTS HAS FAILED TO LOCATE SUCH A COMPONENT. ,   16943
     4 /, 5X,  81HMAYBE THE USER'S DATA IS OUT OF ORDER, OR HE HAS MISSP   16944
     5ELLED THE NAME IN QUESTION.    )                                     16945
      WRITE(LUNIT6, 7131)                                               M17.1582
      GO TO 6220                                                           16947
 6049 IF ( BUS1  .NE.  TRASH )   GO TO 7249                             M22.7260
      WRITE (LUNIT6, 7349)  KSAT, ITYPE                                 M22.7261
 7349 FORMAT ( 5X, 108HTHE USER IS CURRENTLY INPUTTING A SATURABLE TRANSM22.7262
     1FORMER COMPONENT, WHICH BY DEFINITION MUST HAVE TWO OR MORE  ,/,  M22.7263
     2 5X,  86HWINDINGS.   THE LAST-READ DATA CARD SHOULD HAVE BEEN A WIM22.7264
     3NDING CARD FOR WINDING NUMBER,  I4,  20H ,   BUT THIS IS NOT  ,/, M22.7265
     4 5X,  47HWHAT WAS PUNCHED IN COLUMNS  1-2 .   A VALUE OF,         M22.7266
     5 I4,  33H   WAS READ FROM THIS DATA FIELD.   )                    M22.7267
      GO TO 6220                                                        M22.7268
 7249 WRITE (LUNIT6, 7049)  BUS1, LSTAT(16)                             M22.7269
 7049 FORMAT( 5X, 107HTHE USER IS CURRENTLY INPUTTING A SATURABLE TRANSF   16949
     1ORMER, USING THE REFERENCE BRANCH PROCEDURE TO GET A COPY  ,/,       16950
     2 5X,  51HOF THE UNIT BEARING INTERNAL IDENTIFYING BUS NAME ', A6,    16951
     3  23H'.   BUT THAT UNIT HAD , I2,  21H WINDINGS.   THE COIL ,/,      16952
     4 5X, 107HCARDS BEING READ ARE EITHER OUT OF NATURAL ORDER (CHECK C   16953
     5OLUMNS 1-2 SEQUENCE NUMBERS), OR THEY REPRESENT AN  ,/,              16954
     6 5X, 110HINCORRECT NUMBER OF WINDINGS.   RECALL THAT THE WINDING C   16955
     7ARDS MUST BE NUMBERED 1, 2, ETC. ----- CONSECUTIVELY,    ,/,         16956
     8 5X, 111HWITH NO MISSING NUMBERS.   ONCE A BREAK IN THIS NATURAL O   16957
     9RDER HAS BEEN DETECTED FOR THE COLUMN 1-2 NUMBERS, THE   )           16958
      WRITE(LUNIT6, 7149)                                               M17.1584
 7149 FORMAT( 5X, 103HEMTP LOGIC ASSUMES THAT DATA FOR THE TRANSFORMER IM25.4673
     1N QUESTION HAS BEEN COMPLETED.   THIS IS WHY SHUFFLED   ,/,          16961
     2 5X,  61HWINDING CARDS PRODUCE AN 'ERRONEOUS-NUMBER-OF-TURNS' MESS   16962
     3AGE.   )                                                             16963
      GO TO 6220                                                           16964
 6050 WRITE(LUNIT6, 7050 )   BUS6, LSTAT(15)                            M17.1585
 7050 FORMAT( 5X,  92HDATA ERROR HAS BEEN DETECTED WHILE WORKING ON INPU   16966
     1T OF 3-PHASE TRANSFORMER COMPONENT BEARING   ,/,                     16967
     2 5X,  30HINTERNAL BUS-NAME IDENTIFIER ', A6,  62H'.   THE PROGRAM    16968
     3EXPECTS 3 SINGLE-PHASE TRANSFORMER COMPONENTS   ,/,                  16969
     4 5X,  95HTO FOLLOW THE THREE-PHASE CARD, CONTIGUOUSLY.   THE LAST-   16970
     5READ BRANCH HAS BEEN FOUND AFTER ONLY  , I3, /,                      16971
     6 5X,  24HSINGLE-PHASE COMPONENTS.  )                                 16972
 6220 LASTOV = NCHAIN                                                   M15.1679
      NCHAIN = NFRFLD + 50                                              M22.7270
      IF ( IPRSUP  .GE.  1 )                                            M32,2266
     1 WRITE (LUNIT6, 8943)  NCHAIN, KILL                               M39.3662
 8943 FORMAT ( 31H EXIT "SUBR51".  NCHAIN, KILL =,  2I6 )               M39.3663
 9000 RETURN                                                            M39.3664
      END                                                                  16975
