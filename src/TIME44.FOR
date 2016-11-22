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
