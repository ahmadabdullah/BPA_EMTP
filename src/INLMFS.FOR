      SUBROUTINE INLMFS 
      IMPLICIT REAL*8 (A-H, O-Z),  INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR'    
      INCLUDE  'DEKSPY.FOR'    
      CHARACTER*20  ARGINC(35)
      DIMENSION  KARD(200), KARG(200), KBEG(200), KEND(200) 
      DIMENSION  KTEX(200)   
      DIMENSION  KOLINC(35)  
C        character*80  answ80,  file6(500)
C        character*1  char1, digit(10)
C        logical  logvar
      DATA  KOMLEV    /  -1  /
      IF (BUFF77(1:1) .EQ. '$')  GO TO 1788  !1st time in for $INCLUDE
      N16 = KARD(200)
      N26 = 2
C     WRITE(*,*) ' Encoding "C " card, n16=', n16
      IF ( BUFF77(1:2) .EQ. 'C '  )  GO TO 4203  !2ND CONT. CARD
      WRITE(*,*) ' Invalid cont. card for $INCLUDE argument list.'
      STOP
 1788 N26 = 9  
 1789 IF ( BUFF77(N26:N26) .NE. ' '   .AND.  
     1     BUFF77(N26:N26) .NE. ',' )  GO TO 1797  
      N26 = N26 + 1  
      IF ( N26 .LT. 40 )  GO TO 1789
 1794 ISTEP = J
        write (*,*) ' Halt with bad $INCLUDE in INLMFS.'
        stop
 1797 K = N26 + 1 
 1801 IF ( BUFF77(K:K) .EQ. ','  .OR.  
     1     BUFF77(K:K) .EQ. ' ' )  GO TO 1804
      K = K + 1
      IF ( K .GT. 60 )  GO TO 1794  
      GO TO 1801  
 1804 N7 = K - N26
      ANSW80(1:N7) = BUFF77(N26:K-1)
      N8 = N7  
      ANSW80(N8+1:80) = BLAN80(N8+1:80)
 1811 L = NUMCRD  
      N19 = LIMCRD
      J = NUMDCD  
C       write (*,*) ' Move LINE CON..  J, NUMCRD =',  J, NUMCRD
      DO 1816  M=J, NUMCRD  
C      write (*,*) ' Next LINE CONSTANTS card.   M, FILE6(M) =',  
C    1                                           M, FILE6(M)
      FILE6(N19) = FILE6(M)
 1816 N19 = N19 - 1  
C     N19 = N19 + 1  
 1819 WRITE (LUNIT6, 1820)  J, ANSW80(1:N8)  
 1820 FORMAT ( '   --- THL PASS.  CARD =', I4,  
     1         '.   READY TO OPEN $INCLUDE =',  A  )  
      INQUIRE (FILE=ANSW80(1:N8), EXIST=LOGVAR) 
      IF ( LOGVAR )  GO TO 2294  
      write (LUNIT6, 5823)  ANSW80(1:N8)  
 5823 FORMAT ( /, ' Trouble with special  $INCLUDE that',
     1   ' is to service line model frequency scan.'    ,/, 
     2   ' Halt in  INLMFS  called by  OVER2.  Bye, bye!' )  
      STOP  
 2294 PROM80 = FILE6(J) 
      FILE6(J) = 'C '//PROM80(1:78) 
      OPEN (UNIT=LUNT13, STATUS='OLD', FILE=ANSW80(1:N8))
      N16 = 0  
      N26 = K + 1 
 4203 DO 4205  L=N26, 80      ! SEARCH COLS. N26-80 FOR NONBLANK  
      IF ( BUFF77(L:L+1) .EQ. '$$' )  GO TO 2006 ! If continue card, exit
      IF ( BUFF77(L:L) .NE. ','  .AND.    ! IF NOT "," OR BLANK,  
     1     BUFF77(L:L) .NE. ' ' ) GO TO 4208   ! ARGUMENT STARTS  
 4205 CONTINUE      ! END  DO 4205  LOOP;  COL. "L" NOT ARGUMENT  
      GO TO 4226             ! ALL ARGUMENTS FOUND; NOW USE THEM  
 2006 write (*,*) ' Continuation next.   N13, N16, keybrd, INCDAT, L =',
     1                                   N13, N16, keybrd, INCDAT, L 

      KARD(200) = N16     ! Save N16 when '$$' CARD is read
      RETURN    !  go back  to over2 for 2nd 'arg' card for LMFS


C      N13 = N13 + 1 ! Next LUNT10 index below last $INCLUDE: for continu
CCCC  CALL READ10 ( N13, ABUFF ) ! Load ABUFF with LUNT10 card numbr N13
C      BUFF77 = FILE6(N13)     ! TRANSFER TO SCALAR WORKING STORAGE  
C      write (*,*) ' Continue is card  N13, BUFF77 =',  
C     1                                n13, buff77
C      N26 = 2   ! Reset column number to begin search for next ABUFF arg
C      write (*,*) ' Begin arg continuation  BUFF77 =',  BUFF77

C      GO TO 4203   ! Loop back to begin search for args on this continue


 4208 N16 = N16 + 1       ! ANOTHER (THE N16-TH) ARGUMENT BEGINS 
C      IF ( N16 .GT. 10 )  ! IF NUMBER OF ARGUMENTS IS TOO LARGE,   
      IF ( N16 .GT. 18)    ! CURRENT LIMIT OF 18 NODES (9-PHASE)
     1 CALL STOPTP
      N12 = INDEX ( BUFF77(L:), ',' )  
      N13 = INDEX ( BUFF77(L:), ' ' )  
      N14 = N12
      IF ( N12 .GT. 0 ) GO TO 4214  
      N14 = N13
      IF ( N13 .GT. 0 ) GO TO 4220  
      WRITE (MUNIT6, 4211) 
 4211 FORMAT ( ' NO BOUNDING SYMBOL.  STOP AFTER DISPLAY.' )
      CALL WINDOW 
      WRITE (MUNIT6, 4223)  L, N12, N13, N14, N26  
      CALL WINDOW 
      CALL STOPTP 
 4214 IF ( N13 .GT. 0  .AND.  
     1     N13 .LT. N14 ) N14 = N13 
 4220 N15 = N14 - 1  
      KOLINC(N16) = N15 
      N18 = L - 1 + N15 
      ARGINC(N16) = ' ' 
      ARGINC(N16)(1:N15) = BUFF77(L:N18)  
      N26 = N18 + 1  
      IF ( IPRSPY .LT. 5 )  GO TO 4224 
      WRITE (MUNIT6, 4223)  L, N12, N13, N14, N26  
 4223 FORMAT ( ' DONE WITH ARGUMENT.  L, N12, N13, N14, N26 =',
     1   8I6 ) 
      CALL WINDOW 
 4224 GO TO 4203  
 4226 KARD(1) = 999999  
      N1 = 0
      IF ( N16 .EQ. 0 )  GO TO 4239 
 4228 READ (LUNT13, 4232) ( KBEG(L), L=1, 25 )  
      DO 4229  L=1, 25  
      IF ( KBEG(L) .EQ. 0 ) GO TO 4230 
 4229 CONTINUE 
      N1 = N1 + 25
      IF ( N1 .LE. 175 )  GO TO 4228
      CALL STOPTP 
 4230 REWIND LUNT13  
      N6 = N1 + L - 1
      READ (LUNT13, 4232) (KARD(K), K=1, N6) 
      READ (LUNT13, 4232) (KARG(K), K=1, N6) 
      READ (LUNT13, 4232) (KBEG(K), K=1, N6) 
      READ (LUNT13, 4232) (KEND(K), K=1, N6) 
      READ (LUNT13, 4232) (KTEX(K), K=1, N6) 
 4232 FORMAT ( 4X, 25I3 )  
      IF ( IPRSPY .LT. 1 )  GO TO 4235 
      WRITE (MUNIT6, 4233)  N6
 4233 FORMAT ( ' DONE READING ARGUMENT USAGE VECTORS.  N4 =', I5 )
      CALL WINDOW 
 4235 IF ( IPRSPY .LT. 5 )  GO TO 4238 
      WRITE (MUNIT6, 4236) 
 4236 FORMAT ( ' VECTORS KARD, KARG, KBEG, KEND, KTEXT(1:25) ...' )  
      CALL WINDOW 
      WRITE (MUNIT6, 4237)  ( KARD(K), K=1, N5 )
      CALL WINDOW 
      WRITE (MUNIT6, 4237)  ( KARG(K), K=1, N5 )
      CALL WINDOW 
      WRITE (MUNIT6, 4237)  ( KBEG(K), K=1, N5 )
      CALL WINDOW 
      WRITE (MUNIT6, 4237)  ( KEND(K), K=1, N5 )
      CALL WINDOW 
      WRITE (MUNIT6, 4237)  ( KTEX(K), K=1, N5 )
 4237 FORMAT ( 4X, 25I3 )  
      CALL WINDOW 
 4238 KARD(N6+1) = 999999  
 4239 N20 = 0  
      N24 = 1  
   
      N18 = J + 1 
      DO 4273  K=N18, LIMCRD  
      READ (LUNT13, 1329, END=1828)  BUFF77  
 1329 FORMAT ( A80 )                                                    
      IF ( BUFF77(1:1) .NE. 'C' )  GO TO 4247
      DO 4241  L=1, 10  
      IF ( BUFF77(2:2) .EQ. DIGIT(L) )  GO TO 4244 
 4241 CONTINUE 
      GO TO 4247  
 4244 IF ( L .EQ. 10 )  L = 0 
      IF ( L .GT. KOMLEV )  GO TO 4273 
      BUFF77(2:2) = ' ' 
 4247 J = J + 1
      FILE6(J) = BUFF77 
      IF ( BUFF77(1:1) .EQ. 'C' )
     1 GO TO 4273 
      N20 = N20 + 1  
      IF ( IPRSPY .LT. 3 )  GO TO 4249 
      WRITE (MUNIT6, 4248)  N20, N24, KARD(N24) 
 4248 FORMAT ( ' READY WITH NEXT CARD.  N20, N24, KARD(N24) =',  3I8  ) 
      CALL WINDOW 
 4249 IF ( N20 .LT. KARD(N24) ) GO TO 4273
      N1 = KBEG(N24) 
      N2 = KEND(N24) 
      N4 = KARG(N24) 
      N3 = KOLINC(N4)
      IF ( N4 .LE. N16 )  GO TO 34250  
      WRITE (LUNIT6, 4250)  N24, N4, N16  
 4250 FORMAT ( '   ? ? ? ?   ERROR STOP AT S.N. 4250 OF "DATAIN".',  
     1         '   INSUFFICIENT NUMBER OF $INCLUDE ARGUMENTS.' 
     2   ,/,   '             N24, N4, N16 =',  3I8  ) 
      CALL STOPTP 
34250 IF ( N2-N1 .NE. N3-1   .AND.  
     1     KTEX(N24) .EQ. 1 ) 
     2 GO TO 4253 
      IF ( N2-N1 .GE. N3-1 ) GO TO 4261
 4253 WRITE (MUNIT6, 4254)  N4, N20 
 4254 FORMAT ( '   +++ Argument',  I4,  '   length-mismatch error.', 
     1         '   Used on card',  I4,  2H .    )  
      CALL WINDOW 
      WRITE (MUNIT6, 4255)  N24, N1, N2, N3  
 4255 FORMAT ( '       N24 =',  I4,  '    KBEG, KEND =',  2I4, 
     3         '    Length from $INCLUDE =',  I4,  2H .  )  
      CALL WINDOW 
         stop  
 4261 N = N2
      IF ( IPRSPY .LT. 4 ) GO TO 34261 
      WRITE (MUNIT6, 24261)  ARGINC(N4)
24261 FORMAT ( ' ARGUMENT NOW PROCESSED, ARGINC(N4) =',  A20 ) 
      CALL WINDOW 
34261 DO 4263  IP=1, 20 
      M = 21 - IP 
      CHAR1 = ARGINC(N4)(M:M) 
      IF ( CHAR1 .EQ. ' ' )  GO TO 4263
      IF ( IPRSPY .LT. 8 ) GO TO 54262 
      WRITE (MUNIT6, 4262)  IP, M, CHAR1  
 4262 FORMAT ( '  NEXT NON-BLANK DIGIT.  IP, N, DIGIT =',
     1   2I6,  '   "',  A1,  1H"  ) 
      CALL WINDOW 
54262 IF ( CHAR1 .EQ. '#' )
     1 CHAR1 = ' '
      IF ( N .LT. N1 ) GO TO 4253
      FILE6(J)(N:N) = CHAR1
      N = N - 1
 4263 CONTINUE 
      IF ( N+1 .GT. N1  .AND. 
     1     KTEX(N24) .EQ. 1 ) 
     2 GO TO 4253 
      IF ( N .GE. N1 )  
     1 FILE6(J)(N1:N) = ' '
      N24 = N24 + 1  
      GO TO 4249  
 4273 CONTINUE 
       stop 
 1828 CLOSE (UNIT=LUNT13,  STATUS='KEEP') 
      IF ( IPRSPY .LT. 1 ) GO TO 1832  
      WRITE (MUNIT6, 1831)  J, N19  
 1831 FORMAT ( ' DONE WITH DISK FILE (CLOSE).  J, N19 =', 2I8 )
      CALL WINDOW 
 1832 BUFF77(1:32) = 'C END OF $INCLUDE.  FILE NAME = '  
      BUFF77(33:80) = ANSW80(1:48)  
      J = J + 1
      FILE6(J) = BUFF77 
      N13 = LIMCRD + 1
 1833 DO 1835  M=N19, LIMCRD  
      J = J + 1
      N13 = N13 - 1
      FILE6(J) = FILE6(N13)
 1835 CONTINUE 
      NUMCRD = J  
C     write (*,*) ' Exit INLMFS.   NUMDCD, NUMCRD =', 
C    1                             NUMDCD, NUMCRD  
      RETURN
      END
   
