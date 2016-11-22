      SUBROUTINE DATE44 ( A )                                           M40. 523
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C)    THE PURPOSE OF SUBROUTINE  DATE44  IS TO INTERROGATE THE          M15. 476
C)    INSTALLATION CALENDAR, AND RETURN THE CURRENT DATE THROUGH THE    M15. 477
C)    ARGUMENT OF THE SUBROUTINE.   EIGHT BCD CHARACTERS ARE ALLOWED,   M15. 478
C)    WITH THE FIRST (LEFT) FOUR CHARACTERS TO BE PLACED IN  A(1) ,     M15. 479
C)    AND THE FINAL (RIGHT) FOUR PLACED IN  A(2) .   A STATEMENT LIKE   M15. 480
C)             WRITE (LUNIT6, 4041)  A                                  M17. 205
C)        4041 FORMAT ( 1X, 2A4 )                                       M15. 482
C)    THUS OUTPUTS THE CURRENT DATE AS FIRST THE MONTH, THEN THE DAY,   M15. 483
C)    AND FINALLY THE YEAR, SEPARATED BY SLASHES (MM/DD/YY) .           M15. 484
C)    SUBROUTINE  DATE44  IS OF COURSE INSTALLATION DEPENDENT.          M15. 485
C)    EUROPEAN (OR GENERALLY NON-UNITED-STATES, PERHAPS) USERS OF THIS  M15. 486
C)    PROGRAM MAY WANT TO REVERSE THE ORDER OF APPEARANCE OF THE MONTH  M15. 487
C)    AND THE DAY, IN CONFORMITY WITH ESTABLISHED EUROPEAN USAGE.       M15. 488
C     INSTALLATION-DEPENDENT  EMTP  MODULE WRITTEN FOR THE  DEC         M24. 589
C     VAX-11/780.    'IDATE'  IS A  DEC  SYSTEM SUBROUTINE WHICH        M24. 590
C     RETURNS THE MONTH, DAY, AND YEAR (OF CENTURY) AS THREE  INTEGER*2 M24. 591
C     NUMERICAL VALUES.                                                 M24. 592
      REAL*8        A(2)                                                M24. 593
      INTEGER*2  N1, N2, N3                                             M24. 594
      CALL IDATE ( N1, N2, N3 )                                         M24. 595
      N4 = N1 / 10                                                      M24. 596
      N5 = N1 - 10*N4                                                   M24. 597
      N6 = N2 / 10                                                      M24. 598
      N7 = N2 - 10*N6                                                   M24. 599
      ENCODE (4, 1386, A(1) )   N4, N5, N6                              M24. 600
 1386 FORMAT ( 2I1,  1H/,  1I1  )                                       M24. 601
      ENCODE (4, 1394, A(2) )   N7, N3                                  M24. 602
 1394 FORMAT ( I1,  1H/,  I2  )                                         M24. 603
      RETURN                                                            M40. 524
      END                                                               M40. 525
