      SUBROUTINE tread
      IMPLICIT REAL*8 (A-H, O-Z),  INTEGER*4 (I-N)
      INCLUDE  'blkcom.ins'                     ! WSM + THL manual modification for BPA EMTP
      INCLUDE  'tacsto.ins' 
            character*80  dbuff
CCCC      EQUIVALENCE ( MONCAS(109), NOUTPR )   ! WSM + THL manual modification for BPA EMTP
      CHARACTER*6 CHVBAR  
CCCC      EQUIVALENCE ( TEXTA6(17),  CHVBAR )   ! WSM + THL manual modification for BPA EMTP
CCCC      CHARACTER*6 CHRCOM, CHRORI            ! WSM + THL manual modification for BPA EMTP
CCCC      EQUIVALENCE ( TEXTA6(23),  CHRCOM )   ! WSM + THL manual modification for BPA EMTP
      chvbar = '|'                              ! WSM + THL manual modification for BPA EMTP
 9000 CONTINUE
      ctbl(38:38)=char(39)
      ctbl(49:49)=char(47)
      ctbl(65:65)=char(9)
      dptr=cptr
      i0=0
      i1=0
      i3=1
      i4=0
      i5=0
      i6=0
      i7=0
      i8=0
      i9=0
CCCCC      CHRORI=CHRCOM        ! WSM + THL manual modification for BPA EMTP
CCCCC      CHRCOM(3:4)='!!'     ! WSM + THL manual modification for BPA EMTP
 3001 IF(.NOT.(i0.EQ.0 .AND. i1.EQ.0
     1)) GOTO 3000
       CONTINUE
       CALL cimage 
      write (dbuff, 7231) ( abuff(j), j=1, 10 )  ! WSM + THL manual modification for BPA EMTP
 7231 format ( 10a8 )                            ! WSM + THL manual modification for BPA EMTP
       IF(.NOT.(NOUTPR.EQ.0)) GOTO 5151
CCCC    WRITE(unit06,1000) CHVBAR(1:1),dbuff    ! WSM + THL
 1000   FORMAT(51x,a1,a80)
        GOTO 5150
 5151  CONTINUE
 5150  CONTINUE
       IF(.NOT.(dbuff(1:7).EQ.ctbl(143:149)
     1 )) GOTO 5141
        i1=1
        i=0
 3051   IF(.NOT.(i.LT.7)) GOTO 3050
         i=i+1
         csto(cptr+i)=dbuff(i:i)
         GOTO 3051
 3050   CONTINUE
        cptr=cptr+7
        GOTO 5140
 5141  CONTINUE
 5140  CONTINUE
       IF(.NOT.(i1.EQ.0)) GOTO 5001
        i10=80
        i=80
 3041   IF(.NOT.(i.GT.1
     1  .AND. dbuff(i:i).EQ.csto(64)
     2  )) GOTO 3040
         i10=i10-1
         i=i-1
         GOTO 3041
 3040   CONTINUE
        i=0
 3011   IF(.NOT.(i.LT.i10)) GOTO 3010
         i=i+1
         j=ichar(dbuff(i:i))
         IF(.NOT.(j.GE.97 .AND. j.LE.122
     1   )) GOTO 5011
          dbuff(i:i)=char(j-32)
          GOTO 5010
 5011    CONTINUE
 5010    CONTINUE
         GOTO 3011
 3010   CONTINUE
        i6=0
        i=0
 3021   IF(.NOT.(i.LT.i10
     1  .AND. i0.EQ.0)) GOTO 3020
         i=i+1
         i2=0
         char1=dbuff(i:i)
         IF(.NOT.(dbuff(i:i+6).EQ.ctbl(143:149)
     1   .AND. dbuff(i+7:i+7).EQ.csto(64)
     2   )) GOTO 5021
          i0=1
          j=0
 3031     IF(.NOT.(j.LT.7)) GOTO 3030
           j=j+1
           k=i-1+j
           csto(cptr+j)=dbuff(k:k)
           GOTO 3031
 3030     CONTINUE
          cptr=cptr+7
          GOTO 5020
 5021    IF(.NOT.(i4.EQ.1)) GOTO 5022
          IF(.NOT.(dbuff(i:i+14).EQ.ctbl(97:111)
     1    )) GOTO 5111
           i=i+14
           i4=0
           GOTO 5110
 5111     CONTINUE
 5110     CONTINUE
          GOTO 5020
 5022    IF(.NOT.(i5.EQ.1)) GOTO 5023
          IF(.NOT.(dbuff(i:i+9).EQ.ctbl(75:84)
     1    )) GOTO 5041
           i=i+9
           i5=0
           GOTO 5040
 5041     CONTINUE
 5040     CONTINUE
          GOTO 5020
 5023    IF(.NOT.(i6.EQ.1)) GOTO 5024
          IF(.NOT.(char1.EQ.csto(67)
     1    )) GOTO 5051
           i6=0
           GOTO 5050
 5051     CONTINUE
 5050     CONTINUE
          GOTO 5020
 5024    IF(.NOT.(i8.EQ.1)) GOTO 5025
          i2=1
          i3=0
          i7=0
          IF(.NOT.(char1.EQ.csto(38)
     1    )) GOTO 5121
           i8=0
           GOTO 5120
 5121     CONTINUE
 5120     CONTINUE
          GOTO 5020
 5025    IF(.NOT.(i9.EQ.1)) GOTO 5026
          i2=1
          i3=0
          i7=0
          IF(.NOT.(char1.EQ.csto(39)
     1    )) GOTO 5131
           i9=0
           GOTO 5130
 5131     CONTINUE
 5130     CONTINUE
          GOTO 5020
 5026    IF(.NOT.(char1.EQ.csto(38)
     1   )) GOTO 5027
          i2=1
          i3=0
          i7=0
          i8=1
          GOTO 5020
 5027    IF(.NOT.(char1.EQ.csto(39)
     1   )) GOTO 5028
          i2=1
          i3=0
          i7=0
          i9=1
          GOTO 5020
 5028    IF(.NOT.(dbuff(i:i+11).EQ.ctbl(85:96)
     1   )) GOTO 5029
          i=i+11
          i4=1
          GOTO 5020
 5029    IF(.NOT.(dbuff(i:i+6).EQ.ctbl(68:74)
     1   )) GOTO 5030
          i=i+6
          i5=1
          GOTO 5020
 5030    IF(.NOT.(char1.EQ.csto(67)
     1   )) GOTO 5031
          i6=1
          GOTO 5020
 5031    IF(.NOT.(char1.EQ.csto(64)
     1   .OR. char1.EQ.csto(65)
     2   .OR. char1.EQ.csto(62)
     3   .OR. char1.EQ.csto(63)
     4   )) GOTO 5032
          IF(.NOT.(char1.EQ.csto(62)
     1    )) GOTO 5091
           i7=1
           GOTO 5090
 5091     CONTINUE
 5090     CONTINUE
          IF(.NOT.(i3.EQ.0)) GOTO 5061
           i3=1
           i2=1
           char1=csto(64)
           GOTO 5060
 5061     CONTINUE
 5060     CONTINUE
          GOTO 5020
 5032    CONTINUE
          IF(.NOT.((char1.EQ.csto(47)
     1        .OR. char1.EQ.csto(46))
     2    .AND. i7.GT.0)) GOTO 5101
           csto(cptr+1)=csto(62)
           cptr=cptr+1
           GOTO 5100
 5101     CONTINUE
 5100     CONTINUE
          i3=0
          i7=0
          i2=1
 5020    CONTINUE
         IF(.NOT.(i2.EQ.1)) GOTO 5071
          cptr=cptr+1
          csto(cptr)=char1
          GOTO 5070
 5071    CONTINUE
 5070    CONTINUE
         GOTO 3021
 3020   CONTINUE
        IF(.NOT.(csto(cptr).NE.csto(64)
     1  )) GOTO 5081
         i3=1
         cptr=cptr+1
         csto(cptr)=csto(64)
         GOTO 5080
 5081   CONTINUE
 5080   CONTINUE
        GOTO 5000
 5001  CONTINUE
 5000  CONTINUE
       GOTO 3001
 3000 CONTINUE
      cptr=cptr+1
      csto(cptr)=csto(64)
CCCC      CHRCOM=CHRORI          ! WSM + THL manual modification for BPA EMTP
      RETURN
      END
