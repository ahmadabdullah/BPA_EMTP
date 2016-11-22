      SUBROUTINE MAIN10
      IMPLICIT REAL*8 (A-H, O-Z),  INTEGER*4 (I-N)                              
      COMMON  / C0B001 /   X     (   10000 )
      COMMON  / C0B002 /   YKM   (   20000 )
      COMMON  / C0B003 /   KM    (   20000 )
      COMMON  / C0B004 /   XK    (  121080 )
      COMMON  / C0B005 /   XM    (  121080 )
      COMMON  / C0B006 /   WEIGHT(     460 )
      COMMON  / C0B007 /   IWTENT(      31 )
      COMMON  / C0B008 /   CON1  (      30 )
      COMMON  / C0B009 /   ISKIP (       5 )
      COMMON  / C0B010 /   ZINF  (       5 )
      COMMON  / C0B011 /   ETA   (       5 )
      COMMON  / C0B012 /   NHIST (       5 )
      COMMON  / C0B013 /   STAILM(      90 )
      COMMON  / C0B014 /   STAILK(      90 )
      COMMON  / C0B015 /   XMAX  (    3600 )
      COMMON  / C0B016 /   KOUTVP(     508 )
      COMMON  / C0B017 /   BNRG  (     254 )
      COMMON  / C0B018 /   SCONST(  100000 )
      COMMON  / C0B019 /   CNVHST(   53000 )
      COMMON  / C0B020 /   SFD   (    6000 )
      COMMON  / C0B021 /   QFD   (    6000 )
      COMMON  / C0B022 /   SEMAUX(   50000 )
      COMMON  / C0B023 /   IBSOUT(     900 )
      COMMON  / C0B024 /   BVALUE(     900 )
      COMMON  / C0B025 /   SPTACS(   90000 )
      COMMON  / C0B026 /   KSWTYP(    1200 )
      COMMON  / C0B027 /   MODSWT(    1200 )
      COMMON  / C0B028 /   KBEGSW(    1200 )
      COMMON  / C0B029 /   LASTSW(    1200 )
      COMMON  / C0B030 /   KENTNB(    1200 )
      COMMON  / C0B031 /   NBHDSW(    3600 )
      COMMON  / C0B032 /   TOPEN (    3600 )
      COMMON  / C0B033 /   CRIT  (    1200 )
      COMMON  / C0B034 /   KDEPSW(    3600 )
      COMMON  / C0B035 /   TDNS  (    1200 )
      COMMON  / C0B036 /   ISOURC(    1200 )
      COMMON  / C0B037 /   ENERGY(    1200 )
      COMMON  / C0B038 /   IARDUB(    3600 )
      COMMON  / C0B039 /   ARDUBE(    4800 )
      COMMON  / C0B040 /   NONLAD(     300 )
      COMMON  / C0B041 /   NONLE (     300 )
      COMMON  / C0B042 /   VNONL (     300 )
      COMMON  / C0B043 /   CURR  (     300 )
      COMMON  / C0B044 /   ANONL (     300 )
      COMMON  / C0B045 /   VECNL1(     300 )
      COMMON  / C0B046 /   VECNL2(     300 )
      COMMON  / C0B047 /   NAMENL(     300 )
      COMMON  / C0B048 /   VZERO (     300 )
      COMMON  / C0B049 /   ILAST (     300 )
      COMMON  / C0B050 /   NLTYPE(     300 )
      COMMON  / C0B051 /   KUPL  (     300 )
      COMMON  / C0B052 /   NLSUB (     300 )
      COMMON  / C0B053 /   XOPTBR(    3000 )
      COMMON  / C0B054 /   COPTBR(    3000 )
      COMMON  / C0B055 /   CURSUB(     312 )
      COMMON  / C0B056 /   CCHAR (     900 )
      COMMON  / C0B057 /   VCHAR (     900 )
      COMMON  / C0B058 /   GSLOPE(     900 )
      COMMON  / C0B059 /   KTRANS(    3002 )
      COMMON  / C0B060 /   KK    (    3002 )
      COMMON  / C0B061 /   C     (   10000 )
      COMMON  / C0B062 /   TR    (   20000 )
      COMMON  / C0B063 /   TX    (   20000 )
      COMMON  / C0B064 /   R     (   10000 )
      COMMON  / C0B065 /   NR    (    3000 )
      COMMON  / C0B066 /   LENGTH(    3000 )
      COMMON  / C0B067 /   CIK   (    3000 )
      COMMON  / C0B068 /   CI    (    3000 )
      COMMON  / C0B069 /   CK    (    3000 )
      COMMON  / C0B070 /   ISMOUT(    1052 )
      COMMON  / C0B071 /   ELP   (     404 )
      COMMON  / C0B072 /   CU    (      96 )
      COMMON  / C0B073 /   SHP   (    1008 )
      COMMON  / C0B074 /   HISTQ (     504 )
      COMMON  / C0B075 /   ISMDAT(     120 )
      COMMON  / C0B076 /   TEXVEC(    4000 )
      REAL*8            TEXVEC
      COMMON  / C0B077 /   IBRNCH(     900 )
      COMMON  / C0B078 /   JBRNCH(     900 )
      COMMON  / C0B079 /   TSTOP (     100 )
      COMMON  / C0B080 /   NONLK (     300 )
      COMMON  / C0B081 /   NONLM (     300 )
      COMMON  / C0B082 /   SPUM  (   30000 )
      COMMON  / C0B083 /   KKS   (    3002 )
      COMMON  / C0B084 /   KKNONL(   72048 )
      COMMON  / C0B085 /   ZNONL (   72048 )
      COMMON  / C0B086 /   ZNONLB(    3002 )
      COMMON  / C0B087 /   ZNONLC(    3002 )
      COMMON  / C0B088 /   FINIT (    3002 )
      COMMON  / C0B089 /   KSUB  (     312 )
      COMMON  / C0B090 /   MSUB  (     312 )
      COMMON  / C0B091 /   ISUBEG(     304 )
      COMMON  / C0B092 /   LITYPE(    3000 )
      COMMON  / C0B093 /   IMODEL(    3000 )
      COMMON  / C0B094 /   KBUS  (    3000 )
      COMMON  / C0B095 /   MBUS  (    3000 )
      COMMON  / C0B096 /   KODEBR(    3000 )
      COMMON  / C0B097 /   CKI   (    3000 )
      COMMON  / C0B098 /   CKKJM (    3000 )
      COMMON  / C0B099 /   INDHST(    3000 )
      COMMON  / C0B100 /   KODSEM(    3000 )
      COMMON  / C0B101 /   NAMEBR(   18000 )
      COMMON  / C0B102 /   IFORM (     100 )
      COMMON  / C0B103 /   NODE  (     100 )
      COMMON  / C0B104 /   CREST (     100 )
      COMMON  / C0B105 /   TIME1 (     100 )
      COMMON  / C0B106 /   TIME2 (     100 )
      COMMON  / C0B107 /   TSTART(     100 )
      COMMON  / C0B108 /   SFREQ (     100 )
      COMMON  / C0B109 /   KMSWIT(    3600 )
      COMMON  / C0B110 /   NEXTSW(    1200 )
      COMMON  / C0B111 /   RMFD  (       1 )
      COMMON  / C0B112 /   CIKFD (       1 )
      COMMON  / C0B113 /   IMFD  (     600 )
      COMMON  / C0B114 /   TCLOSE(    1200 )
      COMMON  / C0B115 /   ADELAY(    3600 )
      COMMON  / C0B116 /   KPOS  (    1200 )
      COMMON  / C0B117 /   NAMESW(    1200 )
      COMMON  / C0B118 /   E     (    3002 )
      COMMON  / C0B119 /   F     (    3002 )
      COMMON  / C0B120 /   KSSFRQ(    3002 )
      COMMON  / C0B121 /   KODE  (    3002 )
      COMMON  / C0B122 /   KPSOUR(    3002 )
      COMMON  / C0B123 /   VOLTI (    6000 )
      COMMON  / C0B124 /   VOLTK (    3000 )
      COMMON  / C0B125 /   VOLT  (    6000 )
      COMMON  / C0B126 /   BUS   (    3002 )
      REAL*8            BUS   
      CALL SUBR10
      RETURN
      END
      SUBROUTINE OVER29
      IMPLICIT REAL*8 (A-H, O-Z),  INTEGER*4 (I-N)                              
      COMMON  / C29B01 /   KARRAY( 1992869 )
      COMMON  / SPAC01 /   TP    (   30000 )
      COMMON  / SPAC02 /   NORDER(    3002 )
      COMMON  / SPAC03 /   INDEX (    3002 )
      COMMON  / SPAC04 /   DIAG  (    3002 )
      COMMON  / SPAC05 /   DIAB  (    3002 )
      COMMON  / SPAC06 /   SOLR  (    3002 )
      COMMON  / SPAC07 /   SOLI  (    3002 )
      COMMON  / SPAC08 /   ICH1  (    3002 )
      COMMON  / SPAC09 /   BND   (     300 )
      COMMON  / SPAC10 /   ILOC  (   30000 )
      COMMON  / SPAC11 /   GND   (   30000 )
      CALL SUBR29
      RETURN
      END
      SUBROUTINE OVER31
      IMPLICIT REAL*8 (A-H, O-Z),  INTEGER*4 (I-N)                              
      COMMON  / C31B01 /   KARRAY(     300 )
      CALL SUBR31
      RETURN
      END
      SUBROUTINE OVER39
      IMPLICIT REAL*8 (A-H, O-Z),  INTEGER*4 (I-N)                              
      COMMON  / C39B01 /   XDAT  (   10000 )
      COMMON  / C39B02 /   YDAT  (   10000 )
      COMMON  / C39B03 /   APHDAT(   10000 )
      CALL SUBR39
      RETURN
      END
      SUBROUTINE FIXS10
      IMPLICIT REAL*8 (A-H, O-Z),  INTEGER*4 (I-N)                              
      COMMON  / C10B01 /   JNDEX (    3002 )
      COMMON  / C10B02 /   DIAGG (    3002 )
      COMMON  / C10B03 /   DIABB (    3002 )
      COMMON  / C10B04 /   SOLRSV(    3002 )
      COMMON  / C10B05 /   SOLISV(    3002 )
      COMMON  / C10B06 /   GNDD  (   30000 )
      COMMON  / C10B07 /   BNDD  (   30000 )
      COMMON  / C10B08 /   NEKFIX(     100 )
      COMMON  / C10B09 /   FXTEM1(     100 )
      COMMON  / C10B10 /   FXTEM2(     100 )
      COMMON  / C10B11 /   FXTEM3(     100 )
      COMMON  / C10B12 /   FXTEM4(     100 )
      COMMON  / C10B13 /   FXTEM5(     100 )
      COMMON  / C10B14 /   FXTEM6(     100 )
      COMMON  / C10B15 /   FIXBU1(     100 )
      COMMON  / C10B16 /   FIXBU2(     100 )
      COMMON  / C10B17 /   FIXBU3(     100 )
      COMMON  / C10B18 /   FIXBU4(     100 )
      COMMON  / C10B19 /   FIXBU5(     100 )
      COMMON  / C10B20 /   FIXBU6(     100 )
      COMMON  / C10B21 /   FIXBU7(     100 )
      COMMON  / C10B22 /   FIXBU8(     100 )
      COMMON  / C10B23 /   FIXBU9(     100 )
      COMMON  / C10B24 /   FIXB10(     100 )
      COMMON  / C10B25 /   FIXB11(     100 )
      COMMON  / C10B26 /   KNDEX (     100 )
      CALL SUBR10
      RETURN
      END
      SUBROUTINE OVER44
      IMPLICIT REAL*8 (A-H, O-Z),  INTEGER*4 (I-N)                              
      COMMON  / C44B01 /   KARRAY(     300 )
      COMMON  / C44B02 /   P     (   22155 )
      COMMON  / C44B03 /   Z     (   22155 )
      COMMON  / C44B04 /   IC    (     210 )
      COMMON  / C44B05 /   R     (     210 )
      COMMON  / C44B06 /   D     (     210 )
      COMMON  / C44B07 /   GMD   (     210 )
      COMMON  / C44B08 /   X     (     210 )
      COMMON  / C44B09 /   Y     (     210 )
      COMMON  / C44B10 /   TB2   (     210 )
      COMMON  / C44B11 /   ITB3  (     210 )
      COMMON  / C44B12 /   WORKR1(     210 )
      COMMON  / C44B13 /   WORKR2(     210 )
      COMMON  / C44B14 /   TEXT  (     420 )
      REAL*8            TEXT  
      COMMON  / C44B15 /   GD    (    5565 )
      COMMON  / C44B16 /   BD    (    5565 )
      COMMON  / C44B17 /   YD    (    5565 )
      COMMON  / C44B18 /   ITBIC (     211 )
      COMMON  / C44B19 /   TBR   (     211 )
      COMMON  / C44B20 /   TBD   (     211 )
      COMMON  / C44B21 /   TBG   (     211 )
      COMMON  / C44B22 /   TBX   (     211 )
      COMMON  / C44B23 /   TBY   (     211 )
      COMMON  / C44B24 /   TBTB2 (     211 )
      COMMON  / C44B25 /   ITBTB3(     211 )
      COMMON  / C44B26 /   TBTEXT(     211 )
      REAL*8            TBTEXT
      CALL SUBR44
      RETURN
      END
      SUBROUTINE OVER45
      IMPLICIT REAL*8 (A-H, O-Z),  INTEGER*4 (I-N)                              
      COMMON  / C45B01 /   KARRAY(     300 )
      CALL SUBR45
      RETURN
      END
      SUBROUTINE OVER47
      IMPLICIT REAL*8 (A-H, O-Z),  INTEGER*4 (I-N)                              
      COMMON  / C47B01 /   KARRAY(     300 )
      CALL SUBR47
      RETURN
      END
      SUBROUTINE DIMENS ( LSIZE, NCHAIN, BUS1, BUS2 )                           
      IMPLICIT REAL*8 (A-H, O-Z)                                                
      DIMENSION  LSIZE(62)                                                      
      REAL          BUS1, BUS2                                                  
      IF ( NCHAIN  .GE.  29 )   GO TO 2900                                      
      LSIZE( 1)  =    3002                                                      
      LSIZE( 2)  =    3000                                                      
      LSIZE( 3)  =   10000                                                      
      LSIZE( 4)  =     100                                                      
      LSIZE( 5)  =   20000                                                      
      LSIZE( 6)  =    1200                                                      
      LSIZE( 7)  =    4000                                                      
      LSIZE( 8)  =  120000                                                      
      LSIZE( 9)  =     300                                                      
      LSIZE(10)  =     900                                                      
      LSIZE(11)  =     210                                                      
      LSIZE(12)  =     900                                                      
      LSIZE(13)  =       5                                                      
      LSIZE(14)  =     460                                                      
      LSIZE(15)  =      90                                                      
      LSIZE(16)  =      84                                                      
      LSIZE(17)  =       4                                                      
      LSIZE(18)  =     254                                                      
      LSIZE(19)  =   90000                                                      
      LSIZE(20)  =  100000                                                      
      LSIZE(21)  =    3000                                                      
      LSIZE(22)  =   50000                                                      
      LSIZE(23)  =   30000                                                      
      LSIZE(24)  =      24                                                      
      LSIZE(25)  =   30000                                                      
      LSIZE(26)  =    3000                                                      
      LSIZE(27)  =     600                                                      
      LSIZE(28)  =    1080                                                      
      N7 = 28 + 1                                                               
      LSIZE(N7) = 1842869                                                       
      BUS1 =   91205                                                            
      BUS2 =  123093                                                            
      RETURN                                                                    
 2900 IF ( NCHAIN  .GT.  29 )   GO TO 3100                                      
      LSIZE( 1)  =       0                                                      
      LSIZE( 2) =1992869                                                        
      LSIZE( 3)  =      23                                                      
      LSIZE( 4) =  30000                                                        
      LSIZE( 5)  =       1                                                      
      LSIZE( 6) =   3002                                                        
      LSIZE( 7)  =       9                                                      
      LSIZE( 8) =    300                                                        
      RETURN                                                                    
 3100 IF ( NCHAIN  .GT.  31 )   GO TO 3900                                      
      LSIZE( 1)  =       9                                                      
      LSIZE( 2) =    300                                                        
      RETURN                                                                    
 3900 IF ( NCHAIN  .GT.  39 )   GO TO 1000                                      
      LSIZE( 1)  =      71                                                      
      LSIZE( 2) =  10000                                                        
      RETURN                                                                    
 1000 IF ( NCHAIN  .GT.  10 )   GO TO 4400                                      
      LSIZE( 1)  =       1                                                      
      LSIZE( 2) =   3002                                                        
      LSIZE( 3)  =      23                                                      
      LSIZE( 4) =  30000                                                        
      LSIZE( 5)  =       4                                                      
      LSIZE( 6) =    100                                                        
      RETURN                                                                    
 4400 IF ( NCHAIN  .GT.  44 )   GO TO 4500                                      
      LSIZE( 1)  =       9                                                      
      LSIZE( 2) =    300                                                        
      LSIZE( 3)  =      75                                                      
      LSIZE( 4) =  22155                                                        
      LSIZE( 5)  =      71                                                      
      LSIZE( 6) =    210                                                        
      LSIZE( 7)  =      76                                                      
      LSIZE( 8) =    420                                                        
      LSIZE( 9)  =      74                                                      
      LSIZE(10) =   5565                                                        
      LSIZE(11)  =      73                                                      
      LSIZE(12) =    211                                                        
      RETURN                                                                    
 4500 IF ( NCHAIN  .GT.  45 )   GO TO 4700                                      
      LSIZE( 1)  =       9                                                      
      LSIZE( 2) =    300                                                        
      RETURN                                                                    
 4700 IF ( NCHAIN  .GT.  47 )   GO TO 9900                                      
      LSIZE( 1)  =       9                                                      
      LSIZE( 2) =    300                                                        
      RETURN                                                                    
 9900 LSIZE(1) = LOCINT(BUS1) - LOCINT(BUS2)                                    
      RETURN                                                                    
      END                                                                       
