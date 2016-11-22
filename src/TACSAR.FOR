C     000000            DEFINITION OF TABLE NAMES                 000000M22.  15
      DIMENSION ISBLK (1), INSUP (1), JOUT  (1), ICOLCS(1), ILNTAB(1)   M39.  14
      DIMENSION KSUS  (1), IUTY  (1), IVARB (1)                         M39.  15
      DIMENSION RSBLK (1), UD1   (1), XTCS  (1), ATCS  (1), XAR   (1)   M39.  16
      DIMENSION PARSUP(1), AWKCS (1)                                    M39.  17
C     EQUIVALENCING OF SCALARS WHICH ARE TO BE CARRIED BETWEEN MODULES. M37.  31
      EQUIVALENCE (KONSCE, SPTACS(  1)), (KONCUR, SPTACS(  2))          M39.  18
      EQUIVALENCE (KONTOT, SPTACS(  3)), (KOFSCE, SPTACS(  4))          M39.  19
      EQUIVALENCE (KCOLCS, SPTACS(  5)), (KSPVAR, SPTACS(  6))          M39.  20
      EQUIVALENCE (KATCS , SPTACS(  7)), (KONSUP, SPTACS(  8))          M39.  21
      EQUIVALENCE (KPRSUP, SPTACS(  9)), (KIVARB, SPTACS( 10))          M39.  22
      EQUIVALENCE (KALIU , SPTACS( 11)), (KJOUT , SPTACS( 12))          M39.  23
      EQUIVALENCE (KIUTY , SPTACS( 13)), (KUD1  , SPTACS( 14))          M39.  24
      EQUIVALENCE (KAWKCS, SPTACS( 15)), (KXAR  , SPTACS( 16))          M39.  25
      EQUIVALENCE (KXTCS , SPTACS( 17)), (KLNTAB, SPTACS( 18))          M39.  26
      EQUIVALENCE (KISBLK, SPTACS( 19)), (KRSBLK, SPTACS( 20))          M39.  27
      EQUIVALENCE (KKSUS , SPTACS( 21)), (KALKSU, SPTACS( 22))          M39.  28
      EQUIVALENCE (KINSUP, SPTACS( 23))                                 M39.  29
      EQUIVALENCE ( SPTACS(1), ISBLK (1), KSUS  (1), IUTY  (1) )        M38.  32
      EQUIVALENCE ( SPTACS(1), ILNTAB(1), ICOLCS(1), JOUT  (1) )        M39.  30
      EQUIVALENCE ( SPTACS(1), INSUP (1), IVARB (1), PARSUP(1) )        M39.  31
      EQUIVALENCE ( SPTACS(1), RSBLK (1), UD1   (1), XTCS  (1) )        M39.  32
      EQUIVALENCE ( SPTACS(1), ATCS  (1), XAR   (1), AWKCS (1) )        M39.  33
      EQUIVALENCE   ( NUK   , LSTAT(51) ),     ( IA    , LSTAT(52) )    M39.  34
      EQUIVALENCE   ( NSU   , LSTAT(53) ),     ( NIU   , LSTAT(54) )    M22. 210
      EQUIVALENCE   ( NSUP  , LSTAT(55) ),     ( KARG  , LSTAT(56) )    M22. 211
      EQUIVALENCE   ( KPAR  , LSTAT(57) ),     ( KXIC  , LSTAT(58) )    M39.  35
      EQUIVALENCE   ( IOUTCS, LSTAT(59) ),     ( NSUDV , LSTAT(60) )    M39.  36
