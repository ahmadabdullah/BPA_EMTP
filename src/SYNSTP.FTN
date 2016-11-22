      SUBROUTINE synstp
      INCLUDE  'tacsto.ins'
      k=dptr-449
      IF(k.le.1500) k=1501
      WRITE(UNIT06,1000) (csto(i),i=k,dptr)
 1000 FORMAT(//(1X,75A))
      WRITE(UNIT06,1001) isto(ishenv+54)
 1001 FORMAT(1x/
     1       ' Syntax error #',i5/)
      WRITE(UNIT06,1002)
 1002 FORMAT(   
     1  ' Refer to file DRD5:[TACSLIB]SYNSTOP.MSG for interpretation.')
      STOP 'TACS STOP in SYNSTP.'
      END
