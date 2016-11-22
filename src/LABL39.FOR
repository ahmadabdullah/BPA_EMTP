C     BEGIN WITH FUNCTIONS OF  MMODES=18  LIMIT ON MODES:               M32.  17
      COMMON   /COM39/   TIR(18,18), TII(18,18), TDUM(18,18)            M32.  18
      COMMON   /COM39/   MODSKP(2,18), ALINVC(90)                       M32.  19
C     NEXT COME FUNCTIONS OF  MPOLES=100  LIMIT ON POLES:               M32.  20
      COMMON   /COM39/   AKFRAC(100), ALPHAF(100), FCZR(100)            M32.  21
      COMMON   /COM39/   FCPR(100), FCZ(100), FCP(100)                  M32.  22
      COMMON   /COM39/   INDXV(100), XAUXD(200)                         M32.  23
      COMMON   /COM39/   ZOPRAU(400), ZOPRAO(400), AZEPO(400)           M32.  24
C     NEXT  MXCHKR=2*SUM(2**I), I=0,1,..N  WITH 2**N .GE. MPOLES        M32.  25
      COMMON   /COM39/   XCHKRA(255)                                    M32.  26
C     NEXT COME FUNCTIONS OF  MXKNEE=100  LIMIT ON UPS AND DOWNS:       M32.  27
      COMMON   /COM39/   XKNEE(100), NOPRAO(100)                        M32.  28
C     NEXT COME SCALARS, WITH REALS BEFORE INTEGERS (A LA IBM):         M32.  29
      COMMON   /COM39/   HREFLG, APTDEC, GMODE, AMINA1, ONEHAV          M32.  30
      COMMON   /COM39/   ONEQTR, HRFLGR, EPSTOL, REFA,  REFB            VAX.   2
      COMMON   /COM39/   IDEBUG, IFTYPE, LOUT, NDATA, NTOTRA            M32.  32
      COMMON   /COM39/   NZONE, IZONE, NRANGE, MODIFY, NEXMIS           M32.  33
      COMMON   /COM39/   NORMAX, IFWTA, KOUTPR, INELIM, IFPLOT          M32.  34
      COMMON   /COM39/   IFDAT, IECODE, NZEROR, NPOLER, MODESK          M32.  35
      COMMON   /COM39/   METRIK                                         M33.  58
