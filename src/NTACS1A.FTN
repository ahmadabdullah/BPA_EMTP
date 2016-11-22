      SUBROUTINE ntacs1a          ! WSM + THL manual modification for BPA EMTP
      INCLUDE  'tacsto.ins'   
      OPEN(unit09,FILE='tacs.tim',STATUS='OLD')
      READ(unit09,901) ndx0
  901 FORMAT(I1)
      CLOSE(unit09)
      OPEN(unit08,FILE='tacs.rec',  STATUS='NEW',
     1     CARRIAGECONTROL='FORTRAN')
      WRITE(UNIT08,1009)
 1009 FORMAT(1X)
      CALL tread
      CALL calsto
      OPEN(bkfile,ACCESS='DIRECT',RECL=pgsize,STATUS='SCRATCH')
      CALL ptacs
      RETURN
      END
