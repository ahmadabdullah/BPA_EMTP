C     THIS DECK IS USED ONLY BY INTERACTIVE EXECUTION MODULES           M35.  11
C     WHICH BEGIN WITH "EMTSPY".  NOTE "DECK29" WORKING SPACE:          M35.  12
      COMMON /C29B01/  KARRAY(1)  ! "DECK29" STORES PLOT POINTS         M35.  13
      REAL  PLTBUF(1), FORBYT(600)       ! REAL*4 PLOT STORAGE          M38.  45
      EQUIVALENCE  ( PLTBUF(1), KARRAY(1) )  ! /C29B01/ PLOT POINTS     M35.  15
      LOGICAL  LOGVAR   ! LOGICAL VARIABLE FOR "INQUIRE" USAGE          M35.  16
      COMMON  /SPYCOM/  RAMPCN(20), RAMPSL(20), KYRAMP(20)              M35.  17
      COMMON  /SPYCOM/              FENDRP(20), TMINRP, TMAXRP          M36.   3
      COMMON  /SPYCOM/  TBEGRP(20), TENDRP(20), FBEGRP(20)              M35.  18
      COMMON  /SPYCOM/   TBREAK, EPSKON(14)                             M37. 101
C       END REALS, NEXT COME INTEGERS, IN NEW COMMON BLOCK:             M35.  20
      COMMON  /SPYKOM/  INDXRP(20), IVEC(1000), IASCII(1000)            M35.  21
      COMMON  /SPYKOM/  NUMSYM,  JJROLL, ITEXP, LABELS(15)              M35.  22
      COMMON  /SPYKOM/  MAXARG, KILPER, KFILE5, KVERFY, JWORD           M36.   4
      COMMON  /SPYKOM/  IBEGCL, IENDCL, LIDNT1, LIDNT2, NBREAK          M36.   5
      COMMON  /SPYKOM/  LINNOW, LINSPN, NUMCRD, MUNIT5, NUMKEY          M36.   6
      COMMON  /SPYKOM/  INDBUF, INDBEG, MFLUSH, NEWVEC, MAXFLG          M38.  46
      COMMON  /SPYKOM/  KSPSAV, MEMKAR, NOBACK, KSMSPY(3)               M38.  47
      COMMON  /SPYKOM/  LSERLC, KSERLC, KBRSER, LOCKBR, KERASE          M38.  48
      COMMON  /SPYKOM/  KOMADD                                          M39.  42
      COMMON  /SPYKOM/  IPRSPY,  MONITR, MONITS, LOCATE(1000)           M35.  24
      COMMON  /SPYKOM/  NLINE(1000),  KBREAK, LIMBUF, KOLOUT            M37. 104
      COMMON  /SPYKOM/  LIMARR(4), IMIN(55), IMAX(55), NUMEX            M38.  49
      COMMON  /SPYKOM/  LOCOUT(55), INTOUT(55), NEXMOD, NEXTSN          M38.  50
      COMMON  /SPYKOM/  INCHLP, KSYMBL, KOPYIT, KSLOWR, LIMCRD          M35.  26
      COMMON  /SPYKOM/  LOOPRP(20), N10RMP(20), MEMRMP(20)              M35.  27
      COMMON  /SPYKOM/  KONTAC(14), KONADD(14), KBEGTX(85)              M37. 107
      COMMON  /SPYKOM/  KAR1(1), KAR2(2), NUMRMP, LUNTSP, LOGVAR        M35.  28
      DIMENSION   FKAR1(1), FKAR2(1)                                    M35.  29
      EQUIVALENCE (KAR1, FKAR1), (KAR2, FKAR2)                          M35.  30
      CHARACTER*1   CHAR1, FILBYT(1), COL, FILEXT, DIGIT                M35.  31
      CHARACTER*8  SYMB, SYMBRP, JUNKER, TEXPAR, SPYKWD                 M38.  51
      CHARACTER*8  ANSI8, BROBUS                                        M36.   9
      CHARACTER*16  ANSI16                                              M36.  10
      CHARACTER*20  BYTBUF, BYTFND                                      M35.  32
      CHARACTER*32  ANSI32                                              M36.  11
      CHARACTER*35  SPYCD2                                              M36.  12
      CHARACTER*80   BUFF77, FILE6, FILE6B, BLAN80, PROM80              M35.  33
      CHARACTER*80   ANSW80, TEXSPY, ABUFSV                             M38.  52
      CHARACTER*132   MUNIT6, OUTLIN, OUTSAV, HEDING                    M37. 110
      COMMON /SPYF77/  FILEXT(10),  SYMB(1000), COL(25)                 M35.  34
      COMMON /SPYF77/   BYTFND, CHAR1, SYMBRP(20), ABUFSV, JUNKER       M38.  53
      COMMON /SPYF77/   BYTBUF, BUFF77, FILE6B(20), FILE6(30000)        M37. 112
      COMMON /SPYF77/   BLAN80, PROM80, DIGIT(10), TEXPAR(10)           M36.  15
      COMMON /SPYF77/   SPYKWD(75), ANSI8, ANSI16, ANSI32               M38.  54
      COMMON /SPYF77/   SPYCD2, ANSW80, BROBUS, MUNIT6                  M37. 113
      COMMON /SPYF77/   OUTLIN, OUTSAV, HEDING, TEXSPY(1250)            M39.  43
