C     USAGE IS RESTRICTED TO "PLOT" FUNCTION OF "EMTSPY", DUE           M35.  37
C     TO VARIABLE-LENGTH CONFLICT (THIS IS SINGLE-PRECISION).           M35.  38
      REAL*8          DATEPL, TCLOPL,   BUSLST(1), BBUS                 M35.  39
      COMMON /CBLOCK/  DATEPL(2), TCLOPL(2),  BBUS(300)                 M35.  40
      COMMON /CBLOCK/  TMULT, DY, DX, HPI, TSTEP, GXMIN, GXMAX          M35.  41
      COMMON /CBLOCK/  EW(15000), FINFIN, FILL,  FVCOM(50)              M35.  42
      COMMON /CBLOCK/  YYMIN(20), YYMAX(20), TTMIN(20), TTMAX(20)       M35.  43
      COMMON /CBLOCK/  YLEVEL(20), TTLEV(20), DYOLD(20), MLEVEL(20)     M35.  44
      COMMON /CBLOCK/  AAA(20), BBB(20), KP(20), FXREF(25), FYREF(25)   M35.  45
C     EVNBYT PUTS BUSLST ON REAL*8 BYTE BOUNDARY (IBM LEVEL 4 ERROR):   M39.  44
      COMMON /CBLOCK/  EVNBYT, EV(15000), BX(150), VMINR, VMAXR         M39.  45
      DIMENSION  IBSOUT(1), IBRNCH(1), JBRNCH(1)                        M35.  47
      EQUIVALENCE  ( EV(   1), BUSLST(1) ),  ( EV(1001), IBSOUT(1) )    M35.  48
      EQUIVALENCE  ( EV(1301), IBRNCH(1) ),  ( EV(1601), JBRNCH(1) )    M35.  49
      EQUIVALENCE  ( FVCOM( 1),   VMIN ),  ( FVCOM( 2),   VMAX )        M35.  50
      EQUIVALENCE  ( FVCOM( 3),   HMIN ),  ( FVCOM( 4),   HMAX )        M35.  51
      EQUIVALENCE  ( FVCOM( 5),     HA ),  ( FVCOM( 6), TAXISL )        M35.  52
      EQUIVALENCE  ( FVCOM( 7), TOLRCE ),  ( FVCOM( 8),   HTAX )        M35.  53
      EQUIVALENCE  ( FVCOM( 9),   XTIT ),  ( FVCOM(10),   YTIT )        M35.  54
      EQUIVALENCE  ( FVCOM(11), SIZTIT ),  ( FVCOM(12), XSUPER )        M35.  55
      EQUIVALENCE  ( FVCOM(13), YSUPER ),  ( FVCOM(14), SIZSUP )        M35.  56
      EQUIVALENCE  ( FVCOM(15),  FLINE ),  ( FVCOM(16),  SIZID )        M35.  57
      EQUIVALENCE  ( FVCOM(17),    XID ),  ( FVCOM(18),    YID )        M35.  58
      EQUIVALENCE  ( FVCOM(19),   FACT ),  ( FVCOM(20), DXGRD1 )        M35.  59
      EQUIVALENCE  ( FVCOM(21), DYGRD1 ),  ( FVCOM(22), DXGRD2 )        M35.  60
      EQUIVALENCE  ( FVCOM(23), DYGRD2 ),  ( FVCOM(24),  FILL1 )        M35.  61
      EQUIVALENCE  ( FVCOM(25),  FILL2 ),  ( FVCOM(26),     VS )        M35.  62
      EQUIVALENCE  ( FVCOM(27),     VL ),  ( FVCOM(28),     VH )        M35.  63
      EQUIVALENCE  ( FVCOM(29), PAPMAX ),  ( FVCOM(30), TIMBEG )        M35.  64
      EQUIVALENCE  ( FVCOM(31), TIMEND ),  ( FVCOM(32), FHTAX  )        M35.  65
      EQUIVALENCE  ( FVCOM(33), FXSUP  ),  ( FVCOM(34), FYSUP  )        M35.  66
      EQUIVALENCE  ( FVCOM(35), FXTIT  ),  ( FVCOM(36), FYTIT  )        M35.  67
      EQUIVALENCE  ( FVCOM(37), FXID   ),  ( FVCOM(38), FYID   )        M35.  68
      EQUIVALENCE  ( FVCOM(39), FTCARR ),  ( FVCOM(40), VAXISL )        M35.  69
      EQUIVALENCE  ( FVCOM(41), FXNUMV ),  ( FVCOM(42), FXNUMH )        M35.  70
      EQUIVALENCE  ( FVCOM(43), FVAXTT ),  ( FVCOM(44), FXVERT )        M35.  71
      EQUIVALENCE  ( FVCOM(45), FSYMB  ),  ( FVCOM(46), PAPLIM )        M35.  72
      COMMON /CBLOCK/  MMM(20), MSTART(20), NUMPTS(20), KILLPL          M35.  73
      COMMON /CBLOCK/  KSTART(20)                                       M36.  18
      COMMON /CBLOCK/  MPLOT(20), JHMSP, JCHAN, LABRTM(20)              M35.  74
      COMMON /CBLOCK/  JPLT, ICP, ICURSE, MXYPL, INDEXP, IND1           M35.  75
      COMMON /CBLOCK/  NUMFLT, NCUT, NUMTEK, NEWFIL, MU6SAV             M35.  76
      COMMON /CBLOCK/  MCURVE(20), NAMVAR, MFAKE, NUMRAW                M35.  77
      COMMON /CBLOCK/  NCHSUP, NCHVER, MAXEV, KPTPLT                    M35.  78
      COMMON /CBLOCK/  NUMNVO, NV, NC, NUMBRN, NUMOUT                   M35.  79
      COMMON /CBLOCK/  JPLT1, JBEGBV, JBEGBC, LIMFIX, NT2               M35.  80
      COMMON /CBLOCK/  MAXEW,   MAXIP, MSYMBT(20)                       M35.  81
      COMMON /CBLOCK/  L4PLOT, IVCOM(60)                                M35.  82
      EQUIVALENCE  ( IVCOM( 1),  KLEVL ),  ( IVCOM( 2),  KEXTR )        M35.  83
      EQUIVALENCE  ( IVCOM( 3),    IHS ),  ( IVCOM( 4), LU7PLT )        M35.  84
      EQUIVALENCE  ( IVCOM( 5), IPRSRT ),  ( IVCOM( 6), LIMCOL )        M35.  85
      EQUIVALENCE  ( IVCOM( 7), LINEPR ),  ( IVCOM( 8),  IBAUD )        M35.  86
      EQUIVALENCE  ( IVCOM( 9),   LTEK ),  ( IVCOM(10), NUMTIT )        M35.  87
      EQUIVALENCE  ( IVCOM(11), MAXSYM ),  ( IVCOM(12),   MTIT )        M35.  88
      EQUIVALENCE  ( IVCOM(13), MAXISX ),  ( IVCOM(14), MAXISY )        M35.  89
      EQUIVALENCE  ( IVCOM(15), MGRID1 ),  ( IVCOM(16), MGRID2 )        M35.  90
      EQUIVALENCE  ( IVCOM(17), MSUPER ),  ( IVCOM(18),    MID )        M35.  91
      EQUIVALENCE  ( IVCOM(19),  MLINE ),  ( IVCOM(20),  NCUT1 )        M35.  92
      EQUIVALENCE  ( IVCOM(21),  NCUT2 ),  ( IVCOM(22), NSMPLT )        M35.  93
      EQUIVALENCE  ( IVCOM(23),  LSYMB ),  ( IVCOM(24),  NXMAX )        M35.  94
      EQUIVALENCE  ( IVCOM(25),  NYMAX ),  ( IVCOM(26),  LCHID )        M35.  95
      EQUIVALENCE  ( IVCOM(27), NXINCH ),  ( IVCOM(28), NYINCH )        M35.  96
      EQUIVALENCE  ( IVCOM(29),  NXOFF ),  ( IVCOM(30),  NYOFF )        M35.  97
      EQUIVALENCE  ( IVCOM(31),   LOOK ),  ( IVCOM(32), LCHSUP )        M35.  98
      EQUIVALENCE  ( IVCOM(33), LCHTIT ),  ( IVCOM(34), LCHXAX )        M35.  99
      EQUIVALENCE  ( IVCOM(35), LCHYAX ),  ( IVCOM(36),  ITERM )        M35. 100
      EQUIVALENCE  ( IVCOM(37),   LTIC ),  ( IVCOM(38),  IZTIT )        M35. 101
      EQUIVALENCE  ( IVCOM(39),  IZGR1 ),  ( IVCOM(40),  IZGR2 )        M35. 102
      EQUIVALENCE  ( IVCOM(41), LDSHG1 ),  ( IVCOM(42), LDSHG2 )        M35. 103
      EQUIVALENCE  ( IVCOM(43),  IZXAX ),  ( IVCOM(44),  IZYAX )        M35. 104
      EQUIVALENCE  ( IVCOM(45),  NXID6 ),  ( IVCOM(46),  NYID6 )        M35. 105
      EQUIVALENCE  ( IVCOM(47),  NXEND ),  ( IVCOM(48),  NYEND )        M35. 106
      EQUIVALENCE  ( IVCOM(49),   IZID ),  ( IVCOM(50), NXVERN )        M35. 107
      EQUIVALENCE  ( IVCOM(51), ICHREF ),  ( IVCOM(52), ICHEND )        M35. 108
      EQUIVALENCE  ( IVCOM(53), INCHPX ),  ( IVCOM(54), INCHPY )        M35. 109
      EQUIVALENCE  ( IVCOM(55), INWAIT ),  ( IVCOM(56), NOLABL )        M35. 110
      EQUIVALENCE  ( IVCOM(57), LCHFIL ),  ( IVCOM(58), LCHLIM )        M35. 111
      EQUIVALENCE  ( IVCOM(59), MU6STD )                                M35. 112
      CHARACTER*80   FILNAM, ALPHA, BUFFIN, VERTL, HEADL, SEXT          M35. 113
      CHARACTER*30   FILE30                                             M35. 114
      CHARACTER*24   XYTITL, HORZL                                      M37. 115
      CHARACTER*8    TEXTD1, TEXTD2                                     M35. 116
      CHARACTER*8    CURREN, VOLTAG, BRCLAS, CHAR20                     M35. 117
      CHARACTER*8   SLOT1, DATE, TIME                                   M37. 116
      CHARACTER*8        IBUFF,  ANSI, ABUF77, ANPLT                    M35. 119
C       NEXT TYPE KEY CHARACTER KEY WORDS IN COMMON, IN ORDER:          M35. 120
      CHARACTER*8   CHOICE,   STOP,  PURGE,    OUT,   HELP              M35. 121
      CHARACTER*8   SMOOTH,   SIZE,   SHOW, LINEZZ,   PHOTO             M35. 122
      CHARACTER*8   REPEAT,  FLUSH, PLAYBA,    PEN, MULTIP              M35. 123
      CHARACTER*8   OFFSET, LIMITS,  DEBUG,    TEK,  STACK              M35. 124
      CHARACTER*8   PRINTE, METRIC, ALLTIM, COLUMN, SETCOL              M35. 125
      CHARACTER*8   LONGER, AVERAG,  INNER, RESCAL,   LASTPL            M35. 126
      CHARACTER*8    BATCH,  PUNCH, EXTREM,  LEVEL, NOPLOT              M35. 127
      CHARACTER*8   MESSAG,    END, TIMESP,  LABEL, TIMEUN              M35. 128
      CHARACTER*8   CURSOR, XYPLOT,  SLOPE,   BACK, REFILE              M35. 129
      CHARACTER*8   TEXBLK, SETDAT, TEXFNT                              M38.  55
      COMMON  /PLTANS/  ABUF77(10),  ANSI, IBUFF(20), TEXFNT            M38.  56
      COMMON  /PLTANS/     SEXT(6), HEADL, VERTL, BUFFIN                M35. 132
      COMMON  /PLTANS/  SLOT1(20), HORZL(8), DATE, TIME                 M37. 117
      COMMON  /PLTANS/  TEXTD1, TEXTD2, CURREN, VOLTAG, BRCLAS          M35. 134
      COMMON  /PLTANS/  FILNAM, ALPHA, XYTITL, ANPLT(60)                M35. 135
      EQUIVALENCE ( ANPLT( 1), CHOICE ),  ( ANPLT( 2),   STOP )         M35. 136
      EQUIVALENCE ( ANPLT( 3),  PURGE ),  ( ANPLT( 4),    OUT )         M35. 137
      EQUIVALENCE ( ANPLT( 5),   HELP ),  ( ANPLT( 6), SMOOTH )         M35. 138
      EQUIVALENCE ( ANPLT( 7),   SIZE ),  ( ANPLT( 8),   SHOW )         M35. 139
      EQUIVALENCE ( ANPLT( 9), LINEZZ ),  ( ANPLT(10),  PHOTO )         M35. 140
      EQUIVALENCE ( ANPLT(11), REPEAT ),  ( ANPLT(12),  FLUSH )         M35. 141
      EQUIVALENCE ( ANPLT(13), PLAYBA ),  ( ANPLT(14),    PEN )         M35. 142
      EQUIVALENCE ( ANPLT(15), MULTIP ),  ( ANPLT(16), OFFSET )         M35. 143
      EQUIVALENCE ( ANPLT(17), LIMITS ),  ( ANPLT(18),  DEBUG )         M35. 144
      EQUIVALENCE ( ANPLT(19),    TEK ),  ( ANPLT(20),  STACK )         M35. 145
      EQUIVALENCE ( ANPLT(21), PRINTE ),  ( ANPLT(22), METRIC )         M35. 146
      EQUIVALENCE ( ANPLT(23), ALLTIM ),  ( ANPLT(24), COLUMN )         M35. 147
      EQUIVALENCE ( ANPLT(25), SETCOL ),  ( ANPLT(26), LONGER )         M35. 148
      EQUIVALENCE ( ANPLT(27), AVERAG ),  ( ANPLT(28),  INNER )         M35. 149
      EQUIVALENCE ( ANPLT(29), RESCAL ),  ( ANPLT(30),  LASTPL)         M35. 150
      EQUIVALENCE ( ANPLT(31),  BATCH ),  ( ANPLT(32),  PUNCH )         M35. 151
      EQUIVALENCE ( ANPLT(33), EXTREM ),  ( ANPLT(34),  LEVEL )         M35. 152
      EQUIVALENCE ( ANPLT(35), NOPLOT ),  ( ANPLT(36), MESSAG )         M35. 153
      EQUIVALENCE ( ANPLT(37),    END ),  ( ANPLT(38), TIMESP )         M35. 154
      EQUIVALENCE ( ANPLT(39),  LABEL ),  ( ANPLT(40), TIMEUN )         M35. 155
      EQUIVALENCE ( ANPLT(41), CURSOR ),  ( ANPLT(42), XYPLOT )         M35. 156
      EQUIVALENCE ( ANPLT(43),  SLOPE ),  ( ANPLT(44),   BACK )         M35. 157
      EQUIVALENCE ( ANPLT(45), REFILE ),  ( ANPLT(46), TEXBLK )         M35. 158
      EQUIVALENCE ( ANPLT(47), SETDAT )                                 M37. 118
