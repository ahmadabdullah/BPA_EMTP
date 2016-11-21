      SUBROUTINE errstp
      INCLUDE  'tacsto.ins'
      IF(stpl1.GT.50) stpl1=0
      IF(stpl2.GT.50) stpl2=0
      i1=isto(ishenv+61)
      IF(.NOT.(i1.EQ.0)) GOTO 5001
       i1=1
       cbuff(1:1)=csto(64)
       GOTO 5000
 5001 CONTINUE
       i2=isto(ishenv+62)-1
       i3=i2+i1
       i=0
 3001  IF(.NOT.(i.LT.i1)) GOTO 3000
        i=i+1
        cbuff(i:i)=csto(i2+i)
        GOTO 3001
 3000  CONTINUE
 5000 CONTINUE
      WRITE(UNIT06,1001) cbuff(1:i1),
     1              stpflg, stpi1, stpi2, stpi3, stpi4, stpi5,
     2              stpr1, stpr2, stpr3,
     3              ctbl(stpc1 : stpc1+stpl1-1),
     4              ctbl(stpc2 : stpc2+stpl2-1)
 1001 FORMAT(1x//
     d       ' IN: ',A//
     1       ' STOP FLAG # ',i3//
     2       ' I1=',i10/
     3       ' I2=',i10/
     4       ' I3=',i10/
     5       ' I4=',i10/
     6       ' I5=',i10//
     7       ' R1=',g13.6/
     8       ' R2=',g13.6/
     9       ' R3=',g13.6//
     a       ' NAME1=',a/
     b       ' NAME2=',a//)
      WRITE(unit06,1002)
 1002 FORMAT(   /
     1 ' Refer to file DRD5:[TACSLIB]STOP.MSG for interpretation.')
      STOP 'TACS STOP in ERRSTP.'
      END
