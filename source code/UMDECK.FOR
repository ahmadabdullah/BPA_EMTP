      REAL*8        BUSUM                                               M28.  60
      COMMON /UMCOM/ BUSUM(50),PTHETA(3,3),ZTHEVR(3,3)                  M38.  44
      COMMON /UMCOM/ VINP(40), ZTHEVS(40), UMCUR(40), CON(10)           M31.  24
      COMMON /UMCOM/ DUMVEC(40),DUMMAT(3,3),DATE(2),CLOCK(2)            M31.  25
      COMMON /UMCOM/ PI, SROOT2, SROOT3, OMEGRF                         M27.  11
      COMMON /UMCOM/ INPU, NUMBUS, NCLTOT, NDUM(40), INITUM             M32.  12
C     NEXT COME OFFSETS FOR VARIABLE-DIMENSIONING, IN ORDER.            M27.  13
      COMMON /UMCOM/ IUREAC, IUGPAR, IUFPAR, IUHIST, IUUMRP             M33.  57
      COMMON /UMCOM/ IUNOD1, IUNOD2, IUJCLT, IUJCLO                     M27.  15
      COMMON /UMCOM/ IUJTYP, IUNODO, IUJTMT, IUHISM                     M27.  16
      COMMON /UMCOM/ IUOMGM, IUOMLD, IUTHAM, IUREDU                     M27.  17
      COMMON /UMCOM/ IUREDS, IUFLDS, IUFLDR, IUREQU                     M27.  18
      COMMON /UMCOM/ IUFLQS, IUFLQR, IUJCDS, IUJCQS                     M27.  19
      COMMON /UMCOM/ IUFLXD, IUFLXQ, IUNPPA, IUROTM                     M27.  20
      COMMON /UMCOM/ IUNCLD, IUNCLQ, IUJTQO, IUJOMO                     M27.  21
      COMMON /UMCOM/ IUJTHO, IUREQS, IUEPSO, IUDCOE                     M27.  22
      COMMON /UMCOM/ IUKCOI, IUVOLT, IUANGL, IUNODF                     M31.  27
      COMMON /UMCOM/ IUNODM, IUKUMO, IUJUMO, IUUMOU                     M31.  28
C     DONE WITH OFFSETS FOR SUBROUTINE CALLS.                           M27.  24
      COMMON /UMCOM/ NCLFIX, NUMFIX, IOTFIX, IBSFIX, KSUBUM             M27.  25
      COMMON /UMCOM/ NSMACH, ISTART                                     M35.   3
