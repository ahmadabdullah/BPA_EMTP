C     REAL VARIABLES PRECEDE INTEGER ONES          *********************M33.   5
C     AUXILLIARY ARRAYS ( SIZE BASED ON NO. OF WINDINGS = 7 )  ******** M33.  32
      COMMON  /SMACH/   Z(100),X1(36)                                   M37.  97
C     SPY INTERFACE VARIABLES ***************************************** M39.  37
      COMMON  /SMACH/   SMOUTV(15)                                      M39.  38
C     COMPUTATIONAL CONSTANTS  *****************************************M33.  34
      COMMON  /SMACH/   SQRT3,ASQRT3,SQRT32,THTW,ATHTW,RADEG,OMDT       M33.  35
      COMMON  /SMACH/   FACTOM,DAMRAT,DELTA6,OM2,BIN,BDAM               M38.  39
C     INTEGER VARIABLES      *******************************************M33.  37
      COMMON  /SMACH/   MFIRST, NST, ITOLD, IBROLD, NSMOUT, MSMOUT      M39.  39
