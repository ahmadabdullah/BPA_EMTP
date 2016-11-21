      SUBROUTINE DATAIN                                                 M37. 155
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     UNIVERSAL MODULE OF INTERACTIVE EMTP (SPY OF "EMTSPY").           M37. 156
C     IF NON-INTERACTIVE VERSION, MODULE CAN BE DESTROYED.              M37. 157
C     FIRST EMTP DATA INPUT, AND "SPY" CHOICE, ARE MADE HERE.           M37. 158
C     MODULE IS CALLED ONLY BY INSTALLATION-DEPENDENT "EREXIT".         M37. 159
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'DEKSPY.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'   
      COMMON / LINEMODEL / KEXACT, NSOLVE, FMINSV, NUMRUN, nphlmt
      COMMON / LINEMODEL / CHAR80, CHLMFS(18)
      CHARACTER*6 CHLMFS
      CHARACTER*80 CHAR80
      CHARACTER*6  DUMNAM                                               M37. 161
      CHARACTER*12  TYPDAT(18)                                          M37. 162
      CHARACTER*20  ARGINC(35)                                          M37. 163
      CHARACTER*32  FILSAV
      CHARACTER*40  PREFIX, SUFFIX                                      M37. 164
      CHARACTER*80  tank(1000)                                                  
      DIMENSION  KARD(200), KARG(200), KBEG(200), KEND(200)             M37. 165
      DIMENSION  KTEX(200), LENTYP(18)                                  M37. 166
      DIMENSION  KOLINC(35), MODARG(35), KKKDUM(35)                     M37. 167
      DATA  KOMLEV    /  -1  /    ! DEFAULT COMMENT LEVEL (NONE)        M37. 168
      DATA  NCHPRE    /  0  /   ! BEGIN WITH NO FILE NAME PREFIX        M37. 169
      DATA  NCHSUF    /  0  /   ! BEGIN WITH NO FILE NAME SUFFIX        M37. 170
      DATA  DUMNAM   /  'DUM   '  /    ! DEFAULT DUMMY ROOT NAME        M37. 171
      DATA  TYPDAT(1)  / 'REQUEST     ' /,   LENTYP(1)  /  7  /         M37. 172
      DATA  TYPDAT(2)  / 'FUNCTION    ' /,   LENTYP(2)  /  8  /         M37. 173
      DATA  TYPDAT(3)  / 'TACS SOURCE ' /,   LENTYP(3)  / 11  /         M37. 174
      DATA  TYPDAT(4)  / 'SUPPLEMENTAL' /,   LENTYP(4)  / 12  /         M37. 175
      DATA  TYPDAT(5)  / 'TACS OUTPUT ' /,   LENTYP(5)  / 11  /         M37. 176
      DATA  TYPDAT(6)  / 'TACS INITIAL' /,   LENTYP(6)  / 12  /         M37. 177
      DATA  TYPDAT(7)  / 'BRANCH      ' /,   LENTYP(7)  /  6  /         M37. 178
      DATA  TYPDAT(8)  / 'SWITCH      ' /,   LENTYP(8)  /  6  /         M37. 179
      DATA  TYPDAT(9)  / 'SOURCE      ' /,   LENTYP(9)  /  6  /         M37. 180
      DATA  TYPDAT(10) / 'LOAD FLOW   ' /,   LENTYP(10) /  9  /         M37. 181
      DATA  TYPDAT(11) / 'INITIAL     ' /,   LENTYP(11) /  7  /         M37. 182
      DATA  TYPDAT(12) / 'OUTPUT      ' /,   LENTYP(12) /  6  /         M37. 183
      DATA  TYPDAT(13) / 'PLOT        ' /,   LENTYP(13) /  4  /         M37. 184
      DATA  TYPDAT(14) / 'STATISTICS  ' /,   LENTYP(14) / 10  /         M37. 185
      DATA  NUMTYP   /  14  /  ! TOTAL NUMBER OF DATA TYPE NAMES        M37. 186
      DATA  FILSAV  / '                                '  /  ! for LMFS
      IF ( KEXACT .NE. 88333 )  NUMRUN = 0  ! INIT. NUMRUN for LMFS runs
      IF ( KEXACT .EQ. 88333 .AND. NUMRUN .GT. 0 )
     1     GO TO 5266
      if ( numhld .eq. -8899 ) stop                                             
      if ( numhld .eq. 0 ) go to 5244                                           
      do 2847  j=1, numhld                                                      
      if ( tank(j)(1:19)  .EQ.                                                  
     1     'BEGIN NEW DATA CASE'  .AND.                                         
     2     J  .GT.  3 ) GO TO 3389                                              
 2847 file6(J) = tank(J)                                                        
 3389 numcrd = j - 1                                                            
      do 4723  k=j, numhld                                                      
 4723 tank(k-numcrd) = tank(k)                                                  
      numhld = numhld - numcrd                                                  
      if ( numhld .le. 0 ) numhld = -8899                                       
      numdcd = 0                                                                
      go to 1774 
 5266 KCUT = 0
      IF ( FILSAV .EQ. '                                ' )
     1    GO TO 1712   ! data input assigned from outside
      OPEN ( UNIT=MUNIT5,STATUS='OLD',FILE=FILSAV )! reuse input for 
 1712 IF ( LLBUFF .EQ. -3333 ) REWIND MUNIT5   !generating 2nd & 3rd LMFS data
 5244 IF ( LLBUFF .NE. -3333   .AND.   ! 2ND OR LATER PASS, SKIP                
     1 FILE6(NUMCRD+1)(1:4) .NE. 'EOF ' ) GO TO 1708  ! KEYBOARD        M37. 188
      KVERFY = -4545        ! LOCAL FLAG (NO WINDOWS YET OPENED)        M38.4640
      LIMARG = 35   ! DIMENSIONED LIMIT ON ARGUMENTS OF "MODULE"        M37. 189
      NUMDCD = 0   ! SET POINTER AT ZERO (NO "CIMAGE" CALLS YET)        M35. 199
      MUNIT5 = 5   ! I/O CHANNEL FOR "EMTSPY" INPUT (KEYBOARD)          M35. 200
      LIMCRD = 30000  ! PRESENT FIXED LIMIT ON FILE6 OF "DEKSPY"        M37. 206
      N13 = 0               ! INITIALLY ASSUME NO DEBUG PRINTOUT        M35. 202
      NTACS = 0   ! OLD TACS DATA FORMAT                                M42.7096
      CALL DATE44 ( DATE1(1) )   ! FIND CALENDAR DATE AND THE           M37. 207
      CALL TIME44 ( TCLOCK(1) )  ! TIME OF DAY FOR DOCUMENTATION        M37. 208
      CALL INITSP ! INITIALIZE SPY COMMON (DIGIT NEEDED TO SORT)        M37. 209
 1311 WRITE (LUNIT6, 1324)    ! PROMPT USER AT "EMTSPY" KEYBOARD        M38.4641
 1324 FORMAT (  ' EMTP BEGINS.  SEND (SPY, $ATTACH, DEBUG,',            M35. 204
     1          ' HELP, MODULE, JUNK, STOP) :'  )                       M38.4642
      READ (MUNIT5, 1329) BUFF77  ! READ FIRST CARD OF EMTP DATA        M35. 207
 1329 FORMAT ( A80 )                                                    M35. 208
      IF ( BUFF77(1:5) .EQ. 'STOP ' )  CALL STOPTP                      M39.3277
      IF ( BUFF77(1:5) .NE. 'DISK ' )  GO TO 51329                      M38.4643
      MAXZNO = 4545  ! SIGNAL TO APOLLO "SYSDEP" FOR DISK LUNIT6        M38.4644
      GO TO 1311      ! LOOP BACK FOR NON-LUNIT6-DISK EMTP INPUT        M38.4645
51329 IF ( BUFF77(1:7) .EQ. '$ATTACH' ) GO TO 1347  ! BATCH MODE        M38.4646
      IF ( BUFF77(1:5) .NE. 'JUNK ' )  GO TO 1332                       M38.4647
      WRITE (LUNIT6, 1330)                                              M38.4648
 1330 FORMAT ( '   SEND ROOT WORD TO OVER-RIDE "JUNK" FOR',             M38.4649
     1         ' SPY AND PLOT WINDOWS :' )                              M38.4650
      READ (MUNIT5, 1331)  JUNKER   ! READ NEW WINDOW PAD NAME          M38.4651
      BUFF77(1:8) = 'SPY     '   ! IMPLIED COMMAND NEXT SERVICED        M38.4652
 1331 FORMAT ( A8 )                                                     M38.4653
 1332 IF ( KVERFY .EQ. 0 )  GO TO 41332   ! WINDOWS ALREADY OPEN        M38.4654
      KVERFY = -34543   ! FLAG INDICATING DESIRE TO OPEN WINDOWS        M38.4655
      CALL WINDOW        ! OPEN 2 EXTRA WINDOWS FOR SPY AND PLOT        M38.4656
      KVERFY = 0    ! ERASE FLAG, NOW THAT BOTH WINDOWS ARE OPEN        M38.4657
41332 IF ( BUFF77(1:6) .EQ. 'MODULE' )  GO TO 2613                      M39.3278
      IF ( BUFF77(1:5) .NE. 'HELP ' )  GO TO 1342                       M36.  88
      WRITE (MUNIT6, 1333)                                              M36.  89
 1333 FORMAT ( '    Greetings, greetings.  Welcome to the',             M37. 212
     1         ' wonderful new world of interactive'     )              M37. 213
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 214
      WRITE (MUNIT6, 1334)                                              M37. 215
 1334 FORMAT ( '    EMTP execution, observation, and',                  M37. 216
     1         ' control.  After sending  "SPY",  send'   )             M37. 217
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 218
      WRITE (MUNIT6, 1335)                                              M37. 219
 1335 FORMAT ( '    "HELP",  and then  "ALL"  to receive',              M37. 220
     1         ' some 500 lines of instruction.  Also,'  )              M37. 221
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 222
      WRITE (MUNIT6, 1336)                                              M37. 223
 1336 FORMAT ( '    see Section 9 of the Rule Book dated',              M39.3279
     1         ' June, 1984.  Also see  "Apollo".'       )              M39.3280
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 229
      GO TO 1311                                                        M36. 100
 1342 IF ( BUFF77(1:6) .NE. 'DEBUG ' )  GO TO 1347                      M36. 101
      N13 = 99    ! CHANGE DIAGNOSTIC PRINTOUT CONTROL TO "ON"          M35. 211
      GO TO 1311   ! BACK TO PROMPT FOR INTERACTIVE CHOICE AGAIN        M35. 212
 1347 IPRSPY = N13          !  DIAGNOSTIC SPY PRINTOUT (0 OR 99)        M37. 230
      IPRSUP = N13         !  DIAGNOSTIC EMTP PRINTOUT (0 OR 99)        M35. 216
      IPRSOV(1) = N13    ! EMTP DIAGNOSTIC ONLY THRU 1ST OVERLAY        M35. 217
      IF ( BUFF77(1:3) .NE. 'SPY' ) GO TO 1724 ! NOT INTERACTIVE        M37. 231
C     BEGIN INTERACTIVE CONTROL SEQUENCE, LEADING TO "EMTSPY":          M37. 232
      M4PLOT = 1               ! SET FLAG REMEMBERING USE OF SPY        M37. 233
      WRITE (PROM80, 1357)          ! BUILD VERY 1ST SPY PROMPT         M38.4659
 1357 FORMAT ( ' SPY:' )                                                M38.4660
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M38.4661
 1708 IF ( M4PLOT .NE. 1 )  GO TO 2320   ! NON-INTERACTIVE CASE         M36. 105
      LOCKBR = 1              ! FORCED SPY READ WITHIN "FLAGER"         M38.4662
      CALL EMTSPY  ! B4 ANY EMTP COMPUTATION, START SPY DIALOGUE        M38.4663
      IF ( LOCKBR .EQ. 1 ) GO TO 1708     ! SPY LOOP UNTIL "GO"         M38.4664
      GO TO 1774    ! JUMP TO $INCLUDE REMOVAL, THEN EXIT MODULE        M36. 107
C     BEGIN NON-INTERACTIVE VARIABLE INITIALIZATION:                    M35. 219
 1724 M4PLOT = 2  ! NOT INTERACTIVE, AND USE REAL*4 LUNIT4 PLOTS        M35. 220
      LUNT13 = 5    ! INITIALLY ASSUME EXTERNALLY-CONNECTED DATA        M35. 221
      NUMCRD = 1      ! SO FAR, WE HAVE READ ONE INPUT DATA CARD        M35. 222
      FILE6(1) = BUFF77      ! 1ST CARD IMAGE PERMANENTLY STORED        M35. 223
      IF ( BUFF77(1:7) .NE. '$ATTACH' )  GO TO 1753                     M35. 224
C       "$ATTACH,FILENAME,5" USAGE REQUIRES EXTRACTION OF NAME:         M35. 225
      LUNT13 = 13  ! WE WILL INTERNALLY CONNECT DATA TO UNIT 13         M35. 226
      N16 = 0  ! COLUMN WHICH BEGINS FILE NAME IS NOT YET KNOWN         M38.4665
      DO 1732  J=9, 40  ! SEARCH FOR 2ND COMMA IN THESE COLUMNS         M35. 228
      IF ( BUFF77(J:J) .NE. ' '  .AND.     ! IF NONBLANK COLUMN,        M35. 229
     1     N16 .EQ. 0 )  N16 = J       ! AND 1ST, SAVE IT IN N16        M38.4666
      IF ( BUFF77(J:J) .EQ. ',' )  GO TO 1746  ! 2ND COMMA FOUND        M35. 231
 1732 CONTINUE                                                          M35. 232
 1736 WRITE (MUNIT6, 1737)                                              M37. 234
 1737 FORMAT ( '  ----  Illegal file name.  Try again ....' )           M38.4667
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 235
      GO TO 1311      ! LOOP BACK TO OPENING PROMPT (REPEAT IT)         M35. 233
 1746 N14 = J - N16         ! NUMBER OF CHARACTERS IN FILE NAME         M38.4668
      ANSI32(1:N14) = BUFF77(N16:J-1)   ! TRANSFER DISK FILE NAME       M38.4669
      ANSI32(N14+1:32) = BLAN80(N14+1:32)  ! BLANK OUT REMAINDER        M38.4670
      FILSAV = ANSI32(1:32)
      IF ( IPRSUP .GE. 1 )                                              M35. 235
     1 WRITE (LUNIT6, 1752)  ANSI32                                     M38.4671
 1752 FORMAT ( ' EXTRACTED FILE NAME ANSI32(1:32) =', A32 )             M36. 113
      INQUIRE ( FILE=ANSI32, EXIST=LOGVAR ) ! ASK IF FILE EXISTS        M37. 236
      IF ( .NOT. LOGVAR )  GO TO 1736   ! ILLEGAL FILE; REPROMPT        M37. 237
      SPYCD2(1:32) = ANSI32  ! SAVE FILE NAME FOR PRIME "EREXIT"        M37. 238
      OPEN (UNIT=LUNT13, STATUS='OLD', FILE=ANSI32)                     M36. 114
      FILE6(1) = 'C '//BUFF77(1:78)  ! MAKE $ATTACH INTO COMMENT        M37. 239
      KCUT = 0
 1753 CONTINUE
      krdoff = numcrd
      krdcom = 0
      DO 1758  J=1, LIMCRD  ! READ UNTIL AN END-OF-FILE DETECTED        M36. 115
      READ (LUNT13, 1329, END=1766)  FILE6(KRDOFF+J)                    M35. 236
      IF ( KCUT .EQ. 1 )  GO TO 5486
      if ( file6(krdoff+j)(1:2) .eq. 'C ' )
     1 krdcom = krdcom + 1
      if ( file6(KRDOFF+J)(1:19)  .NE.                                          
     1     'BEGIN NEW DATA CASE'  .OR.                                          
     2     J-krdcom  .LE.  3 ) GO TO 1756                                              
      KCUT = 1
 5486 numhld = numhld + 1                                                       
      if ( numhld .gt. 1000 )                                                   
     1 write (munit6,*) ' Input data cards overflow',                           
     2       ' tank(1000).   Halt.'                                             
      if ( numhld .gt. 1000 ) call stoptp                                       
      tank(numhld) = file6(KRDOFF+J)                                            
C     IF ALL EMTP DATA (E.G., "KILL CODES" USE) COMES VIA KEY           M37. 240
C     BOARD, IT IS ENDED WITH "EOF"; WHEN SOLVED, MORE KEYBOARD.        M37. 241
 1756 IF ( FILE6(KRDOFF+J)(1:4) .EQ. 'EOF ' )  GO TO 1766                       
      IF ( KCUT .EQ. 0 )
     1  NUMCRD = NUMCRD + 1     ! ANOTHER INPUT DATA CARD NOW READ      
 1758 CONTINUE
 1760 WRITE (MUNIT6, 1761)  LIMCRD                                      M36. 116
 1761 FORMAT ( '  & & & & &   Input buffer overflow.  Limit =',         M36. 117
     1   I6,   '.   Reject this data, and reprompt ....'  )             M36. 118
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 243
      IF ( LUNT13 .EQ. 13 )  ! IF THIS WAS $ATTACH USAGE, THEN          M36. 119
     1 GO TO 1311    ! LOOP BACK TO START ANEW THE DATA INPUT           M36. 120
      CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD          M37. 244
 1766 NUMCRD = NUMCRD + 1
      WRITE (MUNIT6, 1767)  NUMCRD                                      M36. 122
 1767 FORMAT ( ' DONE READING DISK FILE INTO EMTP CACHE.',              M35. 238
     1         '   NUMCRD =',  I5,  '  CARDS.'  )                       M35. 239
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 245
      IF ( LUNT13 .NE. 5 )  ! IF INTERNALLY-CONNECTED FILE, THEN        M35. 240
     1 CLOSE (UNIT=LUNT13, STATUS='KEEP')        ! DISCONNECT IT        M35. 241
 1774 N22 = 1    ! INITIALIZE PASS NUMBER OF $INCLUDE PROCESSING        M36. 123
      N13 = 1   ! 1ST $INCLUDE MIGHT BE 1ST DATA CARD (DO 1786)         M36. 124
C     BEGIN LOOP TO REPLACE NEXT PRESENTLY-VISIBLE $INCLUDE :           M37. 246
 1776 N17 = 0     ! SWITCH SET SO DO 1816 LOOP EXECUTED 1ST TIME        M37. 247
      DO 1786  J=N13, NUMCRD  ! SEARCH SUSPECT DATA FOR $INCLUDE        M36. 126
 1777 IF ( FILE6(J)(1:1) .NE. '$' ) GO TO 1786  ! SKIP ALL NON-$        M37. 248
      BUFF77 = FILE6(J)     ! TRANSFER TO SCALAR WORKING STORAGE        M37. 249
      IF ( IPRSPY .LT. 3 ) GO TO 41777  ! JUMP AROUND DIAGNOSTIC        M37. 250
      WRITE (MUNIT6, 31777)  J, BUFF77                                  M37. 251
31777 FORMAT ( ' J =',  I4,  '   NEXT $-CARD = ',  A80 )                M37. 252
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 253
41777 IF ( BUFF77(1:8) .EQ. '$INCLUDE' ) GO TO 1787                     M37. 254
      IF ( BUFF77(1:7) .NE. '$PREFIX'   .AND.   ! ONLY PROCESSES        M37. 255
     1     BUFF77(1:7) .NE. '$SUFFIX'   .AND.   ! THESE LIMITED         M37. 256
     2     BUFF77(1:6) .NE. '$LEVEL'    .AND.   ! DOLLAR CARDS          M37. 257
     3     BUFF77(1:6) .NE. '$DUMMY' )  GO TO 1786                      M37. 258
      K = 8     ! BEGIN SEARCHING FOR FILE PRE/SUFFIX IN COL. 8         M37. 259
 1778 IF ( BUFF77(K:K) .NE. ' '  .AND.    ! IF NOT BLANK AND NOT        M37. 260
     1     BUFF77(K:K) .NE. ',' )  GO TO  1779     ! COMMA, EXIT        M37. 261
      K = K + 1  ! NEXT COLUMN TO RIGHT IN SEARCH FOR FILE START        M37. 262
      IF ( K .LT. 80 ) GO TO 1778  !  BACK TO CHECK NEW COLUMN K        M37. 263
      K = 10       ! PRETEND PRE/SUFFIX NAME BEGINS IN COLUMN 10        M37. 264
      L = 4       ! PRETEND PRE/SUFFIX NAME IS 5 CHARACTERS LONG        M37. 265
      IF ( BUFF77(5:7) .EQ. 'FIX' )  GO TO 31779   ! $PRE/SUFFIX        M37. 266
      ISTEP = J      ! BLANK COMMON COMMUNICATION OF CARD NUMBER        M37. 267
      CALL STOPIN       ! ALLOW USER TO CORRECT ERRONEOUS CARD J        M37. 268
      IF ( KILL .GT. 0 )  GO TO 9200    ! ON OUR WAY TO "OVER51"        M37. 269
      GO TO 1777        !  RETURN TO RE-PROCESS CORRECTED CARD J        M37. 270
 1779 IF ( BUFF77(1:7) .NE. '$PREFIX'   .AND.                           M37. 271
     1     BUFF77(1:7) .NE. '$SUFFIX' )  GO TO  1783                    M37. 272
      L = INDEX ( BUFF77(K:), ' ' )  ! ONE COL. PAST END OF NAME        M37. 273
      M = INDEX ( BUFF77(K:), ',' )      ! COL. OF COMMA, IF ANY        M37. 274
      IF ( M .NE. 0  .AND.   ! IF THERE IS A TRAILING COMMA, AND        M37. 275
     1     M .LT. L )        ! IF THIS PRECEDES THE BLANK, THEN         M37. 276
     2 L = M       ! IT BOUNDS USER-SUPPLIED NAME;  BOUND IS "L"        M37. 277
      L = L - 1    ! INDEX FOR FINAL CHARACTER OF PREFIX/SUFFIX         M37. 278
31779 N5 = K - 1 + L     ! END OF PRE/SUFFIX RELATIVE TO COL. 1         M37. 279
      IF ( BUFF77(1:7) .NE. '$PREFIX' )  GO TO  1782                    M37. 280
      PREFIX = BUFF77(K:N5)  ! PERMANENT STORAGE OF NAME PREFIX         M37. 281
      NCHPRE = L        ! LENGTH OF FILE NAME PREFIX NOW STORED         M37. 282
      GO TO  1785   ! DONE PROCESSING $PREFIX CARD; MAKE COMMENT        M37. 283
 1782 SUFFIX = BUFF77(K:N5)  ! PERMANENT STORAGE OF NAME SUFFIX         M37. 284
      NCHSUF = L         ! LENGTH OF FILE NAME SUFFIX NOW STORED        M37. 285
      GO TO  1785   ! DONE PROCESSING $SUFFIX CARD; MAKE COMMENT        M37. 286
 1783 IF ( BUFF77(1:6) .NE. '$LEVEL' )  GO TO 1784                      M37. 287
      PROM80 = BUFF77(K:80)   ! TRANSFER NUMERICAL PART OF CARD         M37. 288
      CALL FREIN1 ( PROM80, KOMLEV ) ! DECODE KOMLEV FROM PROM80        M37. 289
      GO TO  1785   ! DONE PROCESSING $LEVEL CARD;  MAKE COMMENT        M37. 290
 1784 IF ( BUFF77(1:6) .NE. '$DUMMY' )  GO TO 1786                      M37. 291
      DUMNAM(1:3) = BUFF77(K:K+2)   ! STORE NEW 3-CHAR ROOT NAME        M37. 292
      ANSI8(1:3) = BUFF77(K+3:K+5)   ! TRANSFER DIGITS TO CELL 1        M37. 293
      READ (ANSI8, 31784) KNTDUM   ! RE-INITIALIZE SERIALIZATION        M37. 294
31784 FORMAT ( 3I1 )   ! 3-DIGIT DECIMAL SERIALIZATION FOR DUMMY        M37. 295
 1785 FILE6(J) = 'C '//BUFF77(1:78)    ! CONVERT TO COMMENT CARD        M37. 296
 1786 CONTINUE   ! END  DO 1786  CHECK OF DATA CARD J FOR $INCL.        M36. 128
      GO TO 2320      ! DONE WITH INPUT DATA; NO $INCLUDE REMAIN        M36. 129
 1787 N13 = J  ! REMEMBER INDEX OF 1ST $INCLUDE, FOR NEXT SEARCH        M37. 297
 1788 N26 = 9        ! BEGIN LOOKING FOR SEPARATOR IN COLUMN 11         M37. 298
 1789 IF ( BUFF77(N26:N26) .NE. ' '   .AND.                             M36. 135
     1     BUFF77(N26:N26) .NE. ',' )  GO TO 1797                       M36. 136
      N26 = N26 + 1   ! MOVE ONE COLUMN TO RIGHT IN SEARCH              M36. 137
      IF ( N26 .LT. 40 )  GO TO 1789                                    M36. 138
 1794 ISTEP = J      ! BLANK COMMON COMMUNICATION OF CARD NUMBER        M37. 299
      CALL STOPIN       ! ALLOW USER TO CORRECT ERRONEOUS CARD J        M37. 300
      IF ( KILL .GT. 0 )  GO TO 9200   ! ON OUR WAY TO "OVER51"         M37. 301
      GO TO 1776        !  RETURN TO RE-PROCESS CORRECTED CARD J        M37. 302
 1797 K = N26 + 1     ! STARTING CANDIDATE FOR LAST COL. OF NAME        M37. 303
 1801 IF ( BUFF77(K:K) .EQ. ','  .OR.  ! IF COMMA OR BLANK, EXIT        M37. 304
     1     BUFF77(K:K) .EQ. ' ' )  GO TO 1804   ! WITH FILE NAME        M37. 305
      K = K + 1      ! NEXT COL. RIGHT IN SEARCH FOR END OF NAME        M37. 306
      IF ( K .GT. 60 )  GO TO 1794    ! JUMP TO ERROR CORRECTION        M37. 307
      GO TO 1801  ! LOOP BACK TO CONTINUE SEARCH FOR END OF NAME        M37. 308
 1804 IF ( NCHPRE .GT. 0 )    ! IF NON-BLANK PREFIX EXISTS, THEN        M37. 309
     1 ANSW80(1:NCHPRE) = PREFIX(1:NCHPRE)  ! PREFIX BEGINS NAME        M37. 310
      N7 = NCHPRE + K - N26     ! LENGTH OF PREFIX + CENTER NAME        M37. 311
      ANSW80(NCHPRE+1:N7) = BUFF77(N26:K-1)    ! ADD CENTER NAME        M37. 312
      N8 = N7 + NCHSUF    ! LENGTH OF ENTIRE, COMPLETE FILE NAME        M37. 313
      IF ( NCHSUF .GT. 0 )    ! IF NON-BLANK SUFFIX EXISTS, THEN        M37. 314
     1 ANSW80(N7+1:N8) = SUFFIX(1:NCHSUF) ! SUFFIX FINISHES NAME        M37. 315
      ANSW80(N8+1:80) = BLAN80(N8+1:80)   ! BLANK OUT REMAINDER         M38.4672
 1811 IF ( N17 .EQ. 1 ) GO TO 1819  ! 2ND OR LATER PASS, SO SKIP        M36. 149
      L = NUMCRD        ! 1ST CARD COPIED IS BOTTOM OF DATA CASE        M36. 150
      N19 = LIMCRD       ! DESTINATION OF THIS FIRST CARD COPIED        M36. 151
      DO 1816  M=J+1, NUMCRD   ! LOOP OVER ALL DATA BELOW $INCL.        M37. 316
      FILE6(N19) = FILE6(L)  ! MOVE CARD DOWN AS FAR AS POSSIBLE        M36. 153
      L = L - 1       ! PRECEDING DATA CARD IS NEXT TO BE COPIED        M36. 154
 1816 N19 = N19 - 1     ! CORRESPONDING CARD DESTINATION ADDRESS        M36. 155
      N19 = N19 + 1    ! REMEMBER 1ST CARD OF COPY STORED BELOW         M36. 156
 1819 WRITE (LUNIT6, 1820)  N22, J, ANSW80(1:N8)                        M38.4673
 1820 FORMAT ( '   --- PASS', I3,  ',  CARD =', I4,                     M36. 158
     1         '.   READY TO OPEN $INCLUDE =',  A  )                    M37. 318
      INQUIRE (FILE=ANSW80(1:N8), EXIST=LOGVAR)   ! FILE EXISTS?        M37. 320
      IF ( .NOT. LOGVAR )  GO TO 1794  ! ILLEGAL NAME CORRECTION        M37. 321
      PROM80 = FILE6(J)          ! TEMP STORAGE B4 2-BYTE SHIFT         M38.4674
      FILE6(J) = 'C '//PROM80(1:78)   ! MAKE INTO A COMMENT CARD        M38.4675
      OPEN (UNIT=LUNT13, STATUS='OLD', FILE=ANSW80(1:N8))               M37. 324
      N16 = 0       ! SO FAR, NO ARGUMENTS OF $INCLUDE ARE KNOWN        M37. 325
      N26 = K + 1         ! POINT TO "," OR " " ENDING FILE NAME        M37. 326
 4203 DO 4205  L=N26, 80      ! SEARCH COLS. N26-80 FOR NONBLANK        M37. 327
      IF ( BUFF77(L:L) .NE. ','  .AND.    ! IF NOT "," OR BLANK,        M37. 328
     1     BUFF77(L:L) .NE. ' ' ) GO TO 4208   ! ARGUMENT STARTS        M37. 329
 4205 CONTINUE      ! END  DO 4205  LOOP;  COL. "L" NOT ARGUMENT        M37. 330
      GO TO 4226             ! ALL ARGUMENTS FOUND; NOW USE THEM        M37. 331
 4208 N16 = N16 + 1       ! ANOTHER (THE N16-TH) ARGUMENT BEGINS        M37. 332
      IF ( N16 .GT. 10 )  ! IF NUMBER OF ARGUMENTS IS TOO LARGE,        M37. 333
     1 CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD         M37. 334
      N12 = INDEX ( BUFF77(L:), ',' )    ! LOCATE BOUNDING COMMA        M37. 335
      N13 = INDEX ( BUFF77(L:), ' ' )    ! LOCATE BOUNDING BLANK        M37. 336
      N14 = N12           ! ASSUME THERE'S A COMMA, IN THIS COL.        M37. 337
      IF ( N12 .GT. 0 ) GO TO 4214   ! TRAILING COMMA IS PRESENT        M37. 338
      N14 = N13           ! ASSUME THERE'S A BLANK, IN THIS COL.        M37. 339
      IF ( N13 .GT. 0 ) GO TO 4220    ! NO ",", BUT THERE IS " "        M37. 340
      WRITE (MUNIT6, 4211)                                              M37. 341
 4211 FORMAT ( ' NO BOUNDING SYMBOL.  STOP AFTER DISPLAY.' )            M37. 342
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 343
      WRITE (MUNIT6, 4223)  L, N12, N13, N14, N26                       M37. 344
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 345
      CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD          M37. 346
 4214 IF ( N13 .GT. 0  .AND.   ! IF TRAILING BLANK, AND IF BLANK        M37. 347
     1     N13 .LT. N14 ) N14 = N13 ! PRECEDES ",", BLANK BOUNDS        M37. 348
 4220 N15 = N14 - 1   ! NUMBER OF COLUMNS MAKING UP THE ARGUMENT        M37. 349
      KOLINC(N16) = N15     ! REMEMBER THE WIDTH OF ARGUMENT N16        M37. 350
      N18 = L - 1 + N15  ! COL. ENDING ARGUMENT, BASED ON COL. 1        M37. 351
      ARGINC(N16) = ' '     ! BLANK OUT STORAGE B4 ADDING STRING        M37. 352
      ARGINC(N16)(1:N15) = BUFF77(L:N18)     ! REMEMBER ARGUMENT        M37. 353
      N26 = N18 + 1   ! COLUMN TO BEGIN SEARCH FOR NEXT ARGUMENT        M37. 354
      IF ( IPRSPY .LT. 5 )  GO TO 4224  ! JUMP AROUND DIAGNOSTIC        M37. 355
      WRITE (MUNIT6, 4223)  L, N12, N13, N14, N26                       M37. 356
 4223 FORMAT ( ' DONE WITH ARGUMENT.  L, N12, N13, N14, N26 =',         M37. 357
     1   8I6 )                                                          M37. 358
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 359
 4224 GO TO 4203     ! LOOP BACK TO PROCESS NEXT ARGUMENT IF ANY        M37. 360
 4226 KARD(1) = 999999     ! ASSUME NO ARGUMENTS (THIS IS BOUND)        M37. 361
      N1 = 0  ! INITIALIZE OFFSET FOR POINTER VECTOR READS BELOW        M37. 362
      IF ( N16 .EQ. 0 )  GO TO 4239     ! SKIP ARGUMENT POINTERS        M37. 363
 4228 READ (LUNT13, 4232) ( KBEG(L), L=1, 25 )  ! READ NEXT CARD        M37. 364
      DO 4229  L=1, 25  ! SEARCH I3 REPLACEMENT FIELDS FOR BLANK        M37. 365
      IF ( KBEG(L) .EQ. 0 ) GO TO 4230     ! YES, BOUND IS FOUND        M37. 366
 4229 CONTINUE        ! END  DO 4229  LOOP TO BOUND REPLACEMENTS        M37. 367
      N1 = N1 + 25              ! 25 MORE PARAMETER USAGES FOUND        M37. 368
      IF ( N1 .LE. 175 )  GO TO 4228     ! STILL ROOM FOR 25 MORE       M37. 369
C     OVERFLOW.  199 IS MAX NUMBER OF REPLACEMENTS, TEMPORARILY         M37. 370
      CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD          M37. 371
 4230 REWIND LUNT13       ! REWIND $INCLUDE FILE, TO START AGAIN        M37. 372
      N6 = N1 + L - 1      ! NUMBER OF EFFECTIVE ARGUMENT USAGES        M37. 373
      READ (LUNT13, 4232) (KARD(K), K=1, N6)    ! CARD NOS. USED        M37. 374
      READ (LUNT13, 4232) (KARG(K), K=1, N6)    ! ARGUMENTS USED        M37. 375
      READ (LUNT13, 4232) (KBEG(K), K=1, N6)    ! COL. NO. START        M37. 376
      READ (LUNT13, 4232) (KEND(K), K=1, N6)   ! COL. NO. ENDING        M37. 377
      READ (LUNT13, 4232) (KTEX(K), K=1, N6)    ! ALPHANUM. FLAG        M37. 378
 4232 FORMAT ( 4X, 25I3 )                                               M37. 379
      IF ( IPRSPY .LT. 1 )  GO TO 4235  ! JUMP AROUND DIAGNOSTIC        M37. 380
      WRITE (MUNIT6, 4233)  N6                                          M37. 381
 4233 FORMAT ( ' DONE READING ARGUMENT USAGE VECTORS.  N4 =', I5 )      M37. 382
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 383
 4235 IF ( IPRSPY .LT. 5 )  GO TO 4238  ! JUMP AROUND DIAGNOSTIC        M37. 384
      WRITE (MUNIT6, 4236)                                              M37. 385
 4236 FORMAT ( ' VECTORS KARD, KARG, KBEG, KEND, KTEXT(1:25) ...' )     M37. 386
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 387
      WRITE (MUNIT6, 4237)  ( KARD(K), K=1, N5 )                        M37. 388
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 389
      WRITE (MUNIT6, 4237)  ( KARG(K), K=1, N5 )                        M37. 390
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 391
      WRITE (MUNIT6, 4237)  ( KBEG(K), K=1, N5 )                        M37. 392
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 393
      WRITE (MUNIT6, 4237)  ( KEND(K), K=1, N5 )                        M37. 394
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 395
      WRITE (MUNIT6, 4237)  ( KTEX(K), K=1, N5 )                        M37. 396
 4237 FORMAT ( 4X, 25I3 )                                               M37. 397
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 398
 4238 KARD(N6+1) = 999999       ! BOUND (NO MORE ARGUMENT USAGE)        M37. 399
 4239 N20 = 0        ! NO RECORDS READ FROM $INCLUDE FILE SO FAR        M37. 400
      N24 = 1          ! START BY CONSIDERING 1ST ARGUMENT FIRST        M37. 401
      N5 = 0  ! SO FAR, NO "/" CARDS ENCOUNTERED IN INCLUDE FILE        M37. 402
      N18 = J + 1   ! DO-LOOP LIMIT (NEEDED, FOR J CHANGES INSIDE)      M37. 403
      KNTMAX = 0   ! INITIALIZE MAXIMUM COUNTER FOR DUMMY NAMES         M37. 404
      KNTOLD = KNTDUM   ! SAVE SERIALIZE INDEX AT START OF FILE         M37. 405
      DO 4273  K=N18, LIMCRD   ! READ $INCLUDE RECORDS UNTIL EOF        M37. 406
      READ (LUNT13, 1329, END=1828)  BUFF77     ! NEXT DATA CARD        M37. 407
      IF ( BUFF77(1:4) .EQ. '$EOF' ) GO TO 1828  ! EFFECTIVE EOF        M37. 408
      IF ( BUFF77(1:1) .EQ. '/' )  N5 = 1   ! YES, 1 OR MORE "/"        M37. 409
      IF ( BUFF77(1:1) .NE. 'C' )  GO TO 4247  ! ACCEPT NON-COM.        M37. 410
      DO 4241  L=1, 10       ! SEE IF COL. 2 IS ONE OF 10 DIGITS        M37. 411
      IF ( BUFF77(2:2) .EQ. DIGIT(L) )  GO TO 4244  ! YES, DIGIT        M37. 412
 4241 CONTINUE  ! END  DO 4241  LOOP TO CHECK IF COL. 2 IS DIGIT        M37. 413
      GO TO 4247          ! NON-DIGITED COMMENT CARD IS ACCEPTED        M37. 414
 4244 IF ( L .EQ. 10 )  L = 0     ! DIGIT(10) = 0   IS EXCEPTION        M37. 415
      IF ( L .GT. KOMLEV )  GO TO 4273     ! IGNORE COMMENT CARD        M37. 416
      BUFF77(2:2) = ' '        ! BLANK OUT LEVEL-DIGIT IN COL. 2        M37. 417
 4247 J = J + 1       ! ACCEPT THIS DATA CARD; DO NOT DISCARD IT        M37. 418
      FILE6(J) = BUFF77     ! TRANSFER BUFFER TO REGULAR STORAGE        M37. 419
      IF ( BUFF77(1:1) .EQ. 'C' )    ! COMMENT CARDS CAN HAVE NO        M37. 420
     1 GO TO 4273     ! ARGUMENTS, SO BYPASS THE CHECK FOR USAGE        M37. 421
      N20 = N20 + 1   ! NO. OF COUNTABLE $INCLUDE CARD JUST READ        M37. 422
      IF ( IPRSPY .LT. 3 )  GO TO 4249  ! JUMP AROUND DIAGNOSTIC        M37. 423
      WRITE (MUNIT6, 4248)  N20, N24, KARD(N24)                         M37. 424
 4248 FORMAT ( ' READY WITH NEXT CARD.  N20, N24, KARD(N24) =',  3I8  ) M37. 425
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 426
 4249 IF ( N20 .LT. KARD(N24) ) GO TO 4273   ! NO ARGUMENT USAGE        M37. 427
      N1 = KBEG(N24)    ! BEGINNING COLUMN NUMBER OF REPLACEMENT        M37. 428
      N2 = KEND(N24)    ! ENDING    COLUMN NUMBER OF REPLACEMENT        M37. 429
      N4 = KARG(N24)       ! INDEX NUMBER OF ARGUMENT BEING USED        M37. 430
      N3 = KOLINC(N4)     ! LENGTH OF ARGUMENT BEING SUBSTITUTED        M37. 431
      IF ( N4 .LE. N16 )  GO TO 34250   ! LEGAL ARGUMENT REQUEST        M37. 432
      WRITE (LUNIT6, 4250)  N24, N4, N16                                M37. 433
 4250 FORMAT ( '   ? ? ? ?   ERROR STOP AT S.N. 4250 OF "DATAIN".',     M37. 434
     1         '   INSUFFICIENT NUMBER OF $INCLUDE ARGUMENTS.'          M37. 435
     2   ,/,   '             N24, N4, N16 =',  3I8  )                   M37. 436
      CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD          M37. 437
34250 IF ( N4 .GT. 0 )  GO TO 4252    ! USE ARGUMENT OF $INCLUDE        M37. 438
      KNTDUM = KNTOLD - N4  ! SERIALIZATION FOR DUMMY NAME USAGE        M37. 439
      IF ( -N4 .GT. KNTMAX )  KNTMAX = -N4    ! NEW LARGER DUMMY        M37. 440
      WRITE (ANSI8, 4251) KNTDUM  ! CONVERT INTEGER TO CHARACTER        M37. 441
 4251 FORMAT ( I3 )                                                     M37. 442
      IF ( KNTDUM .LT. 100 ) ANSI8(1:1) = '0'  ! 1ST OF 3 DIGITS        M37. 443
      IF ( KNTDUM .LT. 10  ) ANSI8(2:2) = '0'  ! 2ND OF 3 DIGITS        M37. 444
      IF ( N2-N1 .NE. 5 )         ! IF NAME IS NOT 6 CHARACTERS,        M37. 445
     1 CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD         M37. 446
      DUMNAM(4:6) = ANSI8(1:3)  ! ADD SERIALIZATION TO ROOT NAME        M37. 447
      FILE6(J)(N1:N2) = DUMNAM     ! TRANSFER DUMMY NAME TO CARD        M37. 448
      GO TO 4249  ! LOOP BACK TO CONSIDER POSSIBLE NEXT ARGUMENT        M37. 449
 4252 IF ( N2-N1 .NE. N3-1   .AND.   ! IF ARG. LENGTH MISMATCH,         M37. 450
     1     KTEX(N24) .EQ. 1 )        ! & IF THIS IS TEXT, THEN          M37. 451
     2 GO TO 4253                    ! JUMP TO CORRECTION CHANCE        M37. 452
      IF ( N2-N1 .GE. N3-1 ) GO TO 4261   ! ARG. IS SHORT ENOUGH        M37. 453
 4253 WRITE (MUNIT6, 4254)  N4, N20                                     M37. 454
 4254 FORMAT ( '   +++ Argument',  I4,  '   length-mismatch error.',    M37. 455
     1         '   Used on card',  I4,  2H .    )                       M37. 456
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 457
      WRITE (MUNIT6, 4255)  N24, N1, N2, N3                             M37. 458
 4255 FORMAT ( '       N24 =',  I4,  '    KBEG, KEND =',  2I4,          M37. 459
     3         '    Length from $INCLUDE =',  I4,  2H .  )              M37. 460
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 461
      WRITE (PROM80, 4256)                                              M37. 462
 4256 FORMAT ( ' SEND CORRECTED ARGUMENT (STOP) :'  )                   M37. 463
      CALL PROMPT     ! WRITE PROM80 WITH CURSOR CONTROL (NO LF)        M37. 464
      IF ( M4PLOT .EQ. 1 )  GO TO 4259   ! SPY USAGE ALLOWS RECOVERY    M37. 465
      KILL = 79  ! KILL CODE INDICATING INTERACTIVE HOPELESSNESS        M37. 466
      LSTAT(19) = 4259                                                  M37. 467
      GO TO 9200   ! ASSIGN LSTAT(18), THEN NCHAIN=51; THEN EXIT        M37. 468
 4259 READ (MUNIT5, 4260)  ARGINC(N4)    ! READ REVISED ARGUMENT        M37. 469
 4260 FORMAT ( A20 )                                                    M37. 470
      IF ( ARGINC(N4)(1:5) .EQ. 'STOP ' )  ! IF USER SURRENDERS,        M37. 471
     1 CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD         M37. 472
 4261 N = N2      ! INITIALIZE DESTINATION ADDRESS AT RIGHT EDGE        M37. 473
      IF ( IPRSPY .LT. 4 ) GO TO 34261  ! JUMP AROUND DIAGNOSTIC        M37. 474
      WRITE (MUNIT6, 24261)  ARGINC(N4)                                 M37. 475
24261 FORMAT ( ' ARGUMENT NOW PROCESSED, ARGINC(N4) =',  A20 )          M37. 476
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 477
34261 DO 4263  IP=1, 20  ! PROCESS ALL 20 CHARACTERS OF ARGUMENT        M37. 478
      M = 21 - IP    ! PROCESS FROM RIGHT TO LEFT (RIGHT-ADJUST)        M37. 479
      CHAR1 = ARGINC(N4)(M:M)     ! TRANSFER BYTE TO TEMP SCALAR        M37. 480
      IF ( CHAR1 .EQ. ' ' )  GO TO 4263        ! SKIP OVER BLANK        M37. 481
      IF ( IPRSPY .LT. 8 ) GO TO 54262  ! JUMP AROUND DIAGNOSTIC        M37. 482
      WRITE (MUNIT6, 4262)  IP, M, CHAR1                                M37. 483
 4262 FORMAT ( '  NEXT NON-BLANK DIGIT.  IP, N, DIGIT =',               M37. 484
     1   2I6,  '   "',  A1,  1H"  )                                     M37. 485
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 486
54262 IF ( CHAR1 .EQ. '#' )   ! POUNDS RESERVES BLANK SPACE, SO         M37. 487
     1 CHAR1 = ' '            ! IT IS NOW BLANKED AS IT IS USED         M37. 488
      IF ( N .LT. N1 ) GO TO 4253  ! SPACE ERROR; ALLOW CORRECTION      M37. 489
      FILE6(J)(N:N) = CHAR1     ! SUBSTITUTE THIS ARGUMENT BYTE         M37. 490
      N = N - 1      ! BACK UP DESTINATION ADDRESS FOR NEXT BYTE        M37. 491
 4263 CONTINUE           ! END  DO 4263  LOOP OVER ARGINC(20:1)         M37. 492
      IF ( N+1 .GT. N1  .AND.  ! IF BLANK SPACE REMAINS ON LEFT,        M37. 493
     1     KTEX(N24) .EQ. 1 )        ! & IF THIS IS TEXT, THEN          M37. 494
     2 GO TO 4253   ! ALLOW USER CORRECTION OF ERRONEOUS STRING         M37. 495
      IF ( N .GE. N1 )  ! IF DATA FIELD STILL HAS SPACE ON LEFT,        M37. 496
     1 FILE6(J)(N1:N) = ' '     ! THEN BLANK THIS REMAINDER OUT         M37. 497
      N24 = N24 + 1  ! DONE WITH PRESENT REPLACEMENT; ON TO NEXT        M37. 498
      GO TO 4249      ! LOOP BACK TO CONSIDER THIS NEXT ARGUMENT        M37. 499
 4273 CONTINUE       ! END  DO 4273  LOOP READING ALL FILE CARDS        M37. 500
      GO TO 1760    ! STOP AFTER MESSAGE ABOUT BUFFER OVERFLOW          M36. 163
 1828 CLOSE (UNIT=LUNT13,  STATUS='KEEP')                               M36. 164
      IF ( IPRSPY .LT. 1 ) GO TO 1832   ! JUMP AROUND DIAGNOSTIC        M37. 501
      WRITE (MUNIT6, 1831)  J, N19                                      M37. 502
 1831 FORMAT ( ' DONE WITH DISK FILE (CLOSE).  J, N19 =', 2I8 )         M37. 503
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 504
 1832 KNTOLD = KNTOLD + KNTMAX  ! UPDATE DUMMY SERIALIZ. REFERENCE      M37. 505
      IF ( N5 .EQ. 1 )  GO TO 1833   ! "/" USAGE, SO SKIP C-END         M37. 506
      BUFF77(1:32) = 'C END OF $INCLUDE.  FILE NAME = '                 M37. 507
      BUFF77(33:80) = ANSW80(1:48)  ! TRANSFER FILE NAME OF OPEN        M37. 508
      J = J + 1    ! COMMENT CARD JUST BUILT NEEDS STORAGE INDEX        M37. 509
      FILE6(J) = BUFF77          ! TRANSFER TO PERMANENT STORAGE        M37. 510
 1833 IF ( J .GT. N19 )  GO TO 1760       ! OVERFLOW ERROR STOP         M37. 511
      DO 1835  M=N19, LIMCRD   ! COPY LOWER CARDS BACK AS K+1,..        M36. 170
      J = J + 1   ! DESTINATION INDEX FOR NEXT DATA CARD COPIED         M36. 171
      FILE6(J) = FILE6(M)                                               M36. 172
      IF ( FILE6(J)(1:8) .EQ. '$INCLUDE' )  GO TO 1841                  M36. 173
 1835 CONTINUE                                                          M36. 174
      NUMCRD = J     ! NEW NUMBER OF CARDS MAKING UP DATA CASE          M36. 175
      N22 = N22 + 1  ! INCREMENT PASS NUMBER OF $INCLUDE REMOVAL        M36. 176
      GO TO 1776   ! LOOP BACK TO SEE IF ANY MORE $INCLUDE EXIST        M37. 512
 1841 N19 = M + 1     ! LOWER INDEX 1 BEYOND $INCLUDE CONSIDERED        M36. 178
      IF ( IPRSPY .LT. 1 )  GO TO 1847  ! JUMP AROUND DIAGNOSTIC        M37. 513
      WRITE (MUNIT6, 1846)  M                                           M37. 514
 1846 FORMAT ( ' NEXT $INCLUDE RECOGNIZED IN DO 1835.  M =', I6 )       M37. 515
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 516
 1847 BUFF77 = FILE6(J)  ! LOAD WORKING STORAGE USED AT S.N.1788        M37. 517
      N17 = 1          ! SIGNAL TO BYPASS DO 1816 CARDS TRANSFER        M36. 179
      GO TO 1788   ! LOOP BACK TO PROCESS FILE6(J) AS $INCLUDE          M36. 180
C     FOLLOWING CODE PROCESSES ANY $SPY USAGE:                          M36. 181
 2320 J = 1                                                             M36. 182
      N7 = 0                                                            M36. 183
      N17 = 0    ! SO FAR, NO DATA-SORTING REQUESTS ("/") FOUND         M37. 518
 2347 IF ( J .GT. NUMCRD )   GO TO 2415   ! DONE WITH $SPY CARDS        M37. 519
      IF ( FILE6(J)(1:8) .NE. '$SPY    ' )  GO TO 2378                  M36. 185
      N7 = N7 + 1                                                       M36. 186
      IF ( N7 .GT. 9 )        ! IF OVER 9 SUCH $SPY USAGES, THEN        M37. 520
     1 CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD         M37. 521
      FILE6(J) = '$SPY, SPYFILE .DAT, 0,'                               M36. 190
      FILE6(J)(14:14) = DIGIT(N7)                                       M36. 191
      ANSI32 = 'SPYFILE .DAT'                                           M36. 192
      ANSI32(8:8) = DIGIT(N7)                                           M36. 193
      IF ( IPRSUP .LT. 1 )  GO TO 2353  ! JUMP AROUND DIAGNOSTIC        M37. 522
      WRITE (MUNIT6, 2354)  ANSI32                                      M37. 523
 2354 FORMAT ( ' PREPARE TO OPEN FOR $SPY.  ANSI32 =',  A32  )          M36. 196
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 524
 2353 OPEN (UNIT=LUNT13, STATUS='NEW', FILE=ANSI32)                     M37. 525
      DO 2361  K=J+1, NUMCRD                                            M36. 198
      IF ( FILE6(K)(1:7) .NE. '$SPYEND' )  GO TO 2361                   M36. 199
      CLOSE (UNIT=LUNT13)                                               M36. 200
      N24 = K - J                                                       M36. 201
      DO 2355  L=K+1, NUMCRD                                            M36. 202
 2355 FILE6(L-N24) = FILE6(L)                                           M36. 203
      NUMCRD = NUMCRD - N24                                             M36. 204
      WRITE (MUNIT6, 2358)   N24, J, NUMCRD                             M36. 205
 2358 FORMAT ( ' DONE WITH UPWARD SHIFT.  N24, J, NUMCRD =', 3I8 )      M36. 206
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 526
      GO TO 2378                                                        M36. 207
 2361 WRITE (LUNT13, 1329)  FILE6(K)                                    M36. 208
 2378 IF ( FILE6(J)(1:1) .EQ. '/' )  N17 = 1   ! SORTING NEEDED         M37. 527
      IF ( FILE6(J)(1:8) .NE. 'TACS OLD' )                              M42.7097
     1   GO TO 2410                                                     M42.7098
      NTACS = 2       ! FLAG FOR OLD TACS DATA W/O '/' CARDS            M42.7099
      FILE6(J)(1:2) = 'C '                                              M42.7100
 2410 J = J + 1                                                         M42.7101
      GO TO 2347                                                        M36. 210
 2415 IF ( N17 .EQ. 0 )  GO TO 9800  ! NO DATA SORTING ("/")            M37. 529
C     BEGIN CODE TO SORT EMTP DATA ACCORDING TO CLASS :                 M37. 530
      N1 = 0      ! NO CLASS-1  ("REQUEST")   USAGE FOUND SO FAR        M37. 531
      N10 = 0     ! NO CLASS-10 ("LOAD FLOW") USAGE FOUND SO FAR        M37. 532
      N11 = 0     ! NO CLASS-11 ("INITIAL")   USAGE FOUND SO FAR        M37. 533
      N12 = 0        ! INITIALIZE NUMBER OF DATA CLASS ("/") CARDS FOUNDM37. 534
      IF ( IPRSUP .GE. 9 )                                              M37. 535
     1 WRITE (LUNIT6, 2416)  ( J, FILE6(J), J=1, NUMCRD )               M37. 536
 2416 FORMAT ( ' ENTIRE INPUT FILE AS WE START SORTING ...'             M37. 537
     1    ,/,  ( I5, A80 ) )                                            M37. 538
      DO 2436  J=1, NUMCRD   ! SEARCH EACH DATA CARD FOR "/" USAGE      M37. 539
 2418 IF ( FILE6(J)(1:1) .NE. '/' ) GO TO 2431 ! SKIP NON-"/" CARD      M37. 540
      N12 = N12 + 1         ! USE NEXT ROW IN TABLE STORING "/" POINTERSM37. 541
      KSSFRQ(N12) = J      ! REMEMBER CARD NUMBER AT START OF DATA CLASSM37. 542
      KPSOUR(N12) = 0  ! SO FAR, NO ENDING CARD NUMBER OF "/" USE       M37. 543
      IF ( N12 .EQ. 1 ) GO TO 2420    ! NO PREVIOUS "/" TO BOUND        M37. 544
      IF ( KPSOUR(N12-1) .EQ. 0 )     ! IF PREVIOUS "/" USAGE IS        M37. 545
     1 KPSOUR(N12-1) = J-1  ! UNBOUNDED, THEN REMEMBER LAST CARD        M37. 546
 2420 DO 2421  K=1, NUMTYP     ! CHECK FOR EACH POSSIBLE DATA CLASS NAMEM37. 547
      IF ( FILE6(J)(2:13) .EQ. TYPDAT(K) )  GO TO 2428   ! YES, FOUND ITM37. 548
 2421 CONTINUE           ! END  DO 2421  LOOP TO IDENTIFY NAME AFTER "/"M37. 549
      IF ( FILE6(J)(2:10)  .NE.  'TACS DATA' )  GO TO 2424              M42.7102
      K = 6         ! SET 'TACS DATA' TO 'TACS INITIAL' IF              M42.7103
      NTACS = 1     ! NEW TACS DATA FORMAT USED.                        M42.7104
      GO TO 2428    !  NTACS SET TO 1 IF TACS WITH NEW FORMAT           M42.7105
 2424 IF ( MUNIT4 .NE. 1 )    ! IF EXECUTION IS NOT INTERACTIVE,        M42.7106
     1 CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD         M37. 551
      WRITE (MUNIT6, 1737)    ! TELL USER TO SEND CORRECTED NAME        M37. 552
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 553
      READ (MUNIT5, 1329)  FILE6(J)    ! READ CORRECTED "/" CARD        M37. 554
      GO TO 2418     ! LOOK BACK TO RECOGNIZE JUST-READ "/" CARD        M37. 555
 2428 KODE(N12) = K        ! REMEMBER DATA CLASS NUMBER IN SUMMARY TABLEM37. 556
      IF ( K .EQ. 1 )  N1 = 1   ! REMEMBER USAGE OF "/REQUEST"          M37. 557
      IF ( K .EQ. 10 ) N10 = 1  ! REMEMBER USAGE OF "/LOAD FLOW"        M37. 558
      IF ( K .EQ. 11 ) N11 = 1  ! REMEMBER USAGE OF "/INITIAL"          M37. 559
 2431 IF ( N12 .GE. 1  .AND.    ! IF 1 OR MORE "/" FOUND SO FAR,        M37. 560
     1     KPSOUR(N12) .EQ. 0 .AND.  ! & LAST "/" NOT YET ENDED,        M37. 561
     2     FILE6(J)(1:6) .EQ. 'BLANK ' )    ! AND CARD IS BLANK,        M37. 562
     3     KPSOUR(N12) = J - 1       ! THEN BOUND LAST "/" USAGE        M37. 563
 2436 CONTINUE      ! END  DO 2436  LOOP WHICH SETS UP SUMMARY "/" TABLEM37. 564
      IF ( KPSOUR(N12) .EQ. 0 )  KPSOUR(N12) = NUMCRD  ! FINAL BOUND    M37. 565
      L = 0                                                             M42.7107
      IF ( IPRSPY .LT. 2 )  GO TO 12438  ! JUMP AROUND DIAGNOSTIC       M42.7108
      WRITE (MUNIT6, 2438)                                              M37. 567
 2438 FORMAT (  32H     ROW  KSSFRQ  KPSOUR    KODE   )                 M37. 568
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 569
      DO 2443  J=1, N12                                                 M37. 570
      WRITE (MUNIT6, 2442) J, KSSFRQ(J), KPSOUR(J), KODE(J)             M37. 571
 2442 FORMAT ( 4I8 )                                                    M37. 572
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 573
 2443 CONTINUE   ! END  DO 2443  LOOP OVER                              M37. 574
12438 L = L + 1   ! BEGINNING CONVERSION OF OLD TACS DATA FORMAT        M42.7109
      IF ( L .GT. N12 )   GO TO 2444                                    M42.7110
      IF ( KODE(L)  .LE. 1  .OR.  KODE(L) .GT. 6)  GO TO 12438          M42.7111
      IF ( NTACS .EQ. 1 )  GO TO 12438                                  M42.7112
      NN1 = KSSFRQ(L) + 1    !  1ST CARD OF CLASS L                     M42.7113
      NN2 = KPSOUR(L)        !  LAST CARD OF CLASS L                    M42.7114
      IF ( KODE(L)  .GT. 2 )  GO TO 12448                               M42.7115
      DO 12440  LL = NN1, NN2                                           M42.7116
      IF ( FILE6(LL)(1:2)  .NE. '99' )  GO TO 12440  ! TYPE '99'        M42.7117
      FILE6(LL)(1:2) = '  '    ! FUNCTION CARDS BECOME '  '             M42.7118
12440 CONTINUE                                                          M42.7119
      GO TO 12438                                                       M42.7120
12448 IF ( KODE(L) .GT. 3 )  GO TO 12458                                M42.7121
      DO 12456  LL = NN1, NN2    ! FOR TACS SOURCES, CONVERT            M42.7122
      IF ( FILE6(LL)(1:1) .EQ. '$' .OR. FILE6(LL)(1:1) .EQ. '9'         M42.7123
     1    .OR.  FILE6(LL)(1:1) .EQ. 'C' )  GO TO 12456                  M42.7124
      IF ( FILE6(LL)(2:2) .NE. '1' )  GO TO 12450  ! TYPE '01'          M42.7125
      FILE6(LL)(1:1) = '1'   ! TO '11',  '02' TO '14', '03' TO          M42.7126
      GO TO 12456            ! '23' AND '04' TO '24'                    M42.7127
12450 IF ( FILE6(LL)(2:2) .NE. '2' )  GO TO 12452                       M42.7128
      FILE6(LL)(1:2)  = '14'                                            M42.7129
      GO TO 12456                                                       M42.7130
12452 FILE6(LL)(1:1)  = '2'                                             M42.7131
12456 CONTINUE                                                          M42.7132
      GO TO 12438                                                       M42.7133
12458 IF ( KODE(L) .GT. 4 )  GO TO 12468                                M42.7134
      GO TO 12438  ! NO CHANGE ON SUPPLEMENTAL VARIABLE DATA            M42.7135
12468 IF ( KODE(L) .GT. 5 )  GO TO 12478                                M42.7136
      DO 12470  LL = NN1, NN2                                           M42.7137
      IF ( FILE6(LL)(1:1) .NE. ' ' )  GO TO 12470                       M42.7138
      FILE6(LL)(1:2) = '33'  ! SET TYPE '  ' TO '33' FOR TACS           M42.7139
12470 CONTINUE               ! OUTPUT CARDS                             M42.7140
      GO TO 12438                                                       M42.7141
12478 DO 12480 LL = NN1, NN2                                            M42.7142
      IF ( FILE6(LL)(1:1) .NE. ' ' )  GO TO 12480                       M42.7143
      FILE6(LL)(1:2) = '77'   ! SET TYPE '  ' TO '77' FOR TACS          M42.7144
12480 CONTINUE                ! INITIAL CONDITION CARDS                 M42.7145
      GO TO 12438                                                       M42.7146
 2444 N17 = 1     ! INITIALIZE ENTRY OF "/" SUMMARY TABLE WE'LL HIT NEXTM37. 575
      N18 = KSSFRQ(N17)       ! INITIALIZE CARD NUMBER WHERE THESE BEGINM37. 576
      N8 = 0   ! INITIALIZE "/" INDEX TO REMOVE POSSIBLE GARBAGE        M37. 577
      N24 = NUMCRD   ! INITIALIZE DESTINATION ADDRESS (1 B4 1ST)        M37. 578
      J = 0     !  INITIALIZE CARD NUMBER OF ORIGINAL DATA DONE         M37. 579
      IF ( FILE6(1)(1:9) .NE. 'C $ATTACH' )  GO TO 2445                 M37. 580
      N24 = N24 + 1    ! INCREMENT DESTINATION ADDRESS PAST THIS        M37. 581
      FILE6(N24) = FILE6(1)    ! TRANSFER CASE-MARKER CARD BELOW        M37. 582
      J = 1      ! UPDATE NUMBER OF ORIGINAL DATA CARDS NOW DONE        M37. 583
 2445 IF ( FILE6(J+1)(1:16) .NE. 'BEGIN NEW DATA C' ) GO TO 2446        M37. 584
      N24 = N24 + 1    ! INCREMENT DESTINATION ADDRESS PAST THIS        M37. 585
      FILE6(N24) = FILE6(J+1)  ! TRANSFER CASE-MARKER CARD BELOW        M37. 586
      J = J + 1  ! UPDATE NUMBER OF ORIGINAL DATA CARDS NOW DONE        M37. 587
 2446 IF ( N1 .EQ. 0 )  GO TO 2453    ! NO "/REQUEST" CARDS USED        M37. 588
      N8 = 1       !  INDEX FOR "REQUEST", DATA WHICH WE NOW ADD        M37. 589
      GO TO 2472    ! JUMP TO THE INSERTION OF THIS CLASS-1 DATA        M37. 590
 2453 J = J + 1   ! NEXT CARD NUMBER OF ORIGINAL DATA CONSIDERED        M37. 591
      IF ( J .GT. NUMCRD )  GO TO 2497      ! DONE WITH ALL DATA        M37. 592
      IF ( J .NE. N18 )  GO TO 2456     ! NOT A "/" CARD, SO BYPASS SUCHM37. 593
      J = KPSOUR(N17)   ! ADVANCE CARD POINTER TO LAST CARD IN "/" CLASSM37. 594
      N17 = N17 + 1   ! FOLLOWING "/" USAGE IS IN THIS NEXT ROW OF TABLEM37. 595
      N18 = KSSFRQ(N17)   ! CARD NUMBER WHERE NEXT "/" CARDS WILL APPEARM37. 596
      GO TO 2453         ! LOOP BACK TO CONTINUE SEARCH FOR "BLANK" CARDM37. 597
 2456 IF ( N8 .NE. 9 )  GO TO 2458   ! NOT AT END OF SOURCE DATA        M37. 598
      IF ( N10 .EQ. 0 )  GO TO 2460   ! NO "/LOAD FLOW" REQUESTS        M37. 599
      N8 = N10   !  INDEX FOR "LOAD FLOW", DATA WHICH WE NOW ADD        M37. 600
      GO TO 2472   ! JUMP TO THE INSERTION OF THIS CLASS-10 DATA        M37. 601
 2458 IF ( N8 .NE. 10 )  GO TO 2462  ! "/INITIAL" NOT JUST ADDED        M37. 602
 2460 IF ( N11 .EQ. 0 ) GO TO 2462   ! NO "/INITIAL" TO BE ADDED        M37. 603
      N8 = 11      !  INDEX FOR "INITIAL", DATA WHICH WE NOW ADD        M37. 604
      GO TO 2472   ! JUMP TO THE INSERTION OF THIS CLASS-11 DATA        M37. 605
 2462 IF ( FILE6(J)(1:6) .NE. 'BLANK ' ) GO TO 2493   ! NOT END OF CLASSM37. 606
      IF ( IPRSPY .LT. 2 )  GO TO 2465  ! JUMP AROUND DIAGNOSTIC        M37. 607
      WRITE (MUNIT6, 2464)  J                                           M37. 608
 2464 FORMAT (  ' BLANK CARD RECOGNIZED.  J =',  I6 )                   M37. 609
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 610
 2465 DO 2466  K=1, NUMTYP    ! SEARCH CLASS TABLE TO IDENTIFY DATA TYPEM37. 611
      N8 = LENTYP(K)   ! NUMBER OF CHARACTERS IN K-TH KEY WORD          M37. 612
      N14 = INDEX ( FILE6(J), TYPDAT(K)(1:N8) ) ! SEARCH FOR KEY WORD K M37. 613
      IF ( N14 .GT. 0 ) GO TO 2469   ! YES, ONE OF OUR DATA CLASSES FOUNM37. 614
 2466 CONTINUE       ! END  DO 2464  LOOP SEEKING TO IDENTIFY BLANK CARDM37. 615
      N14 = INDEX( FILE6(J), 'TACS DATA' )                              M42.7147
      IF ( N14 .EQ. 0 )  GO TO 2493    ! ONLY ONE CLASS IF NEW TACS     M42.7148
      K = 6            ! DATA FORMAT, SO SET CLASS NO. TO 6 (LAST ONE)  M42.7149
 2469 N8 = K       ! DATA CLASS WHICH THIS BLANK CARD TERMINATES        M37. 617
C     CONVERT END OF CLASS BLANK CARD IN OLD TACS DATA TO COMMENT CARD  M42.7150
      IF ( K .EQ. 2  .OR. K .EQ. 3  .OR. K .EQ. 4  .OR.                 M42.7151
     1     K .EQ. 5  )    FILE6(J)(1:2) = 'C '                          M42.7152
 2472 IF ( IPRSPY .LT. 3 )  GO TO 2475  ! JUMP AROUND DIAGNOSTIC        M37. 618
      WRITE (MUNIT6, 2474)  N8                                          M37. 619
 2474 FORMAT ( ' DATA CLASS NEEDS CONSIDERATION.  N8 =',  I6 )          M37. 620
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 621
 2475 DO 2484  K=1, N12   ! SEARCH ROWS OF "/" TABLE FOR CLASS NUMBER N8M37. 622
      IF ( KODE(K) .NE. N8 )  GO TO 2484   ! NOT CORRECT CLASS FOR BLANKM37. 623
      N5 = KSSFRQ(K) + 1  ! BEGINNING ACTIVE DATA CARD OF K-TH "/" USAGEM37. 624
      N6 = KPSOUR(K)                ! ENDING DATA CARD OF K-TH "/" USAGEM37. 625
      IF ( N24+N6-N5 .GE. LIMCRD ) GO TO 1760 ! OVERFLOW MESSAGE        M37. 626
      DO 2477  L=N5, N6   ! LOOP OVER EACH ACTIVE CARD OF K-TH "/" USAGEM37. 627
      N24 = N24 + 1         ! NEXT DESTINATION CARD ADDRESS WE COPY INTOM37. 628
 2477 FILE6(N24) = FILE6(L)      ! INSERT "/" CARD IN LOWER WORKING AREAM37. 629
      IF ( IPRSPY .LT. 3 )  GO TO 2484  ! JUMP AROUND DIAGNOSTIC        M37. 630
      WRITE (MUNIT6, 2479)  N5, N6, N24                                 M37. 631
 2479 FORMAT ( ' DONE WITH "/" COPY BELOW.  N5, N6, N24 =',  3I6 )      M37. 632
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 633
 2484 CONTINUE    ! END  DO 2484  LOOP OVER ALL ENTRIES "K" IN "/" TABLEM37. 634
      IF ( N8 .EQ. 10  .OR.  ! IF JUST DONE WITH "/LOAD FLOW" OR        M37. 635
     1     N8 .EQ. 11 ) GO TO 2456  ! "/INITIAL", SPECIAL RETURN        M37. 636
      IF ( J .LE. 2 )  GO TO 2453  ! "/REQUEST" PRECEDED NOTHING        M37. 637
 2493 N24 = N24 + 1 ! NEXT DESTINATION CARD ADDRESS WE COPY INTO        M37. 638
      FILE6(N24) = FILE6(J)     ! COPY ORIGINAL CARD J INTO LOW ASSEMBLYM37. 639
      IF ( N24 .GE. LIMCRD ) GO TO 1760 ! OVERFLOW ERROR MESSAGE        M37. 640
      IF ( J .LT. NUMCRD )  GO TO 2453   ! NOT FINISHED; BACK FOR NEXT JM37. 641
 2497 IF ( IPRSPY .LT. 2 )  GO TO 2500  ! JUMP AROUND DIAGNOSTIC        M37. 642
      WRITE (MUNIT6, 2499)  N24                                         M37. 643
 2499 FORMAT ( ' DONE WITH LOWER ASSEMBLY.  N24 =',  I8 )               M37. 644
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 645
 2500 J = 0         ! INITIALIZE CARD DESTINATION INDEX FOR UPWARD SHIFTM37. 646
      DO 2505  K=NUMCRD+1, N24    ! CONSIDER EACH LOWER-ASSEMBLED CARD KM37. 647
      J = J + 1      ! FINAL UPPER INDEX, WHERE CARD IN LOWER ROW K GOESM37. 648
      FILE6(J) = FILE6(K)    ! SHIFT CARD IMAGE UPWARD TO FINAL POSITIONM37. 649
      IF ( IPRSPY .LT. 9 )  GO TO 2505  ! JUMP AROUND DIAGNOSTIC        M37. 650
      WRITE (MUNIT6, 2501)  J, FILE6(J)                                 M37. 651
 2501 FORMAT ( ' CARD',  I3,  1H.,  A80 )                               M37. 652
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 653
 2505 CONTINUE                                                          M37. 654
      NUMCRD = J    ! FINAL LENGTH OF EMTP INPUT DATA, AFTER "/" REMOVALM37. 655
      IF ( IPRSPY .LT. 1 )  GO TO 2509  ! JUMP AROUND DIAGNOSTIC        M37. 656
      WRITE (MUNIT6, 2508)  NUMCRD                                      M37. 657
 2508 FORMAT (  ' DONE WITH ALL "/" PROCESSING.  NUMCRD =', I8 )        M37. 658
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 659
 2509 IF ( M4PLOT .EQ. 1 )    ! ?????  IF INTERACTIVE USE,              M37. 660
     1 CALL SPYING    !  ????????   TEMPORARY DIAGNOSTIC  ??????        M37. 661
      GO TO 9800         ! EXIT MODULE AFTER POSSIBLE DIAGNOSTIC        M37. 662
C     BEGIN CODE TO PROCESS "MODULE" OF USER.  CONVERT USER DATA        M37. 663
C     FROM  "ARG", "DUM", AND "NUM" CHARACTER DECLARATIONS TO           M37. 664
C     THE VECTORS KARD/KARG/KBEG/KEND/KTEX AS NEEDED BY $INCLUDE        M37. 665
 2613 IPRSPY = N13          ! DIAGNOSTIC SPY PRINTOUT (0 OR 99)         M37. 666
      NUMCRD = 0     ! ERASE MEMORY OF ANY PREVIOUS DATA STORAGE        M37. 667
      CALL SPYING        ! ALLOW "DATA" USAGE TO READ INPUT FILE        M37. 668
      N8 = 0     ! INITIALIZE THE NUMBER OF DECLARATION CARDS RECOGNIZEDM37. 669
      NUMARG = 0          ! INITIALIZE THE NUMBER OF NON-DUMMY ARGUMENTSM37. 670
      N11 = LIMCRD + 1    ! INITIALIZE DESTINATION ADDRESS 1 PAST BOTTOMM37. 671
      N13 = 0           ! INITIALIZE COUNT OF CARD BEING CONSIDERED NEXTM37. 672
      N4 = 1      ! TEMPORARY INTEGER VARIABLE STORAGE FOR UNITY        M37. 673
      CALL COPYI ( N4, MODARG(1), LIMARG )     ! INITIALIZE MODE        M37. 674
      GO TO 2628              ! JUMP INTO LOOP OVER ALL INPUT CARDS N13 M37. 675
 2621 N11 = N11 - 1    ! NEXT DESTINATION ADDRESS (STORE FROM BOTTOM UP)M37. 676
      FILE6(N11) = BUFF77         ! COPY NON-DECLARATION CARD DOWN BELOWM37. 677
 2628 N13 = N13 + 1         ! NEXT INPUT CARD (NATURAL ORDER) CONSIDEREDM37. 678
      IF ( N13 .GT. NUMCRD )  GO TO 2703     ! DONE PROCESSING ALL CARDSM37. 679
      BUFF77 = FILE6(N13)         ! COPY INPUT CARD INTO WORKING STORAGEM37. 680
      IF ( BUFF77(1:3) .NE. 'ARG'  .AND.            ! SKIP OVER ANY CARDM37. 681
     1     BUFF77(1:3) .NE. 'DUM'  .AND.            ! NOT "ARG" OR "DUM"M37. 682
     2     BUFF77(1:3) .NE. 'NUM' )  GO TO 2621     ! OR "NUM"          M37. 683
      K = 4                                                             M37. 684
      N8 = N8 + 1       ! ONE MORE DECLARATION PUSHED TO TOP OF VECTOR  M37. 685
      FILE6(N8) = BUFF77      ! STORE DECLARATION AFTER LAST SUCH CARD  M37. 686
 2637 IF ( BUFF77(K:K) .NE. ' '  .AND.       ! START OF NEXT ARGUMENT ISM37. 687
     1     BUFF77(K:K) .NE. ',' )  GO TO 2648        ! NON-"," & NON-" "M37. 688
      K = K + 1     ! MOVE ONE BYTE TO RIGHT IN SEARCH FOR NEXT ARGUMENTM37. 689
      IF ( K .LE. 80 ) GO TO 2637     ! LOOP BACK TO TRY THIS NEW COLUMNM37. 690
      GO TO 2628     ! DONE WITH THIS CARD, SO LOOP BACK TO READ ANOTHERM37. 691
 2648 L = K + 1           ! INITIALIZE SEARCH FOR RIGHT EDGE OF ARGUMENTM37. 692
 2656 IF ( BUFF77(L:L) .EQ. ' '  .OR.           ! BLANK OR COMMA BOUNDS M37. 693
     1     BUFF77(L:L) .EQ. ',' )  GO TO 2664   ! RIGHT EDGE OF ARGUMENTM37. 694
      L = L + 1        ! MOVE ONE BYTE TO RIGHT IN SEARCH FOR RIGHT EDGEM37. 695
      IF ( L .LE. 80 )  GO TO 2656    ! LOOP BACK TO TRY THIS NEW COLUMNM37. 696
 2664 L = L - 1    ! RIGHT EDGE OF ARGUMENT IS ONE BYTE TO LEFT OF BOUNDM37. 697
      IF ( BUFF77(1:3) .NE. 'NUM' )  GO TO 2687                         M37. 698
      DO 2672  M=1, NUMARG  ! SEARCH EXISTING ARGUMENT LIST FOR THIS ONEM37. 699
      N6 = L - K + 1  ! NUMBER OF CHARACTERS IN ARGUMENT JUST IDENTIFIEDM37. 700
      IF ( KOLINC(M) .NE. N6 )  GO TO 2672       ! WRONG LENGTH; SKIP ITM37. 701
      IF ( ARGINC(M)(1:N6) .NE. BUFF77(K:L) )  GO TO 2672  ! NO MATCH   M37. 702
      MODARG(M) = 0    ! INDICATE NUMERIC STORAGE (RIGHT ADJUST)        M37. 703
      GO TO 2695          ! EXIT LOOP; ON TO NEXT ARGUMENT ON "NUM" CARDM37. 704
 2672 CONTINUE               ! END  DO 2672  CARD SEARCHING FOR ARGUMENTM37. 705
      WRITE (MUNIT6, 2679) BUFF77(K:L)                                  M37. 706
 2679 FORMAT (  '   ???  ILLEGAL "NUM" DECLARATION.  UNRECOGNIZED',     M37. 707
     1          ' NAME = ',  A )                                        M37. 708
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 709
      CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD          M37. 710
 2687 NUMARG = NUMARG + 1        ! INDEX FOR THIS INPUT ARGUMENT STORAGEM37. 711
      IF ( NUMARG .LE. LIMARG )  GO TO 2692   ! NO OVERFLOW YET         M37. 712
      WRITE (MUNIT6, 2689)  LIMARG                                      M37. 713
 2689 FORMAT ( ' OVERFLOW ERROR STOP.  ARGUMENT USAGE IS',              M37. 714
     1         ' LIMITED IN NUMBER TO LIMARG =',  I5  )                 M37. 715
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 716
      CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD          M37. 717
 2692 ARGINC(NUMARG) = BUFF77(K:L)     ! STORE NEWLY-IDENTIFIED ARGUMENTM37. 718
      KOLINC(NUMARG) = L - K + 1               ! BYTE LENGTH OF ARGUMENTM37. 719
      IF ( BUFF77(1:3) .EQ. 'DUM' )   KKKDUM(NUMARG) = 1                M37. 720
 2695 K = L + 1   ! NEXT LEFT EDGE COULD BE 1 BEYOND PRESENT RIGHT EDGE M37. 721
      GO TO 2637        ! LOOP BACK TO IDENTIFY NEXT ARGUMENT OF BUFF77 M37. 722
 2703 N11 = N11 - 1       ! NEXT DESTINATION ADDRESS FOR $EOF WE CREATE M37. 723
      WRITE (ANSI32, 2705)  DATE1, TCLOCK                               M37. 724
 2705 FORMAT ( 2A4, 2X, 2A4 )                                           M37. 725
      FILE6(N11) = '$EOF   USER-SUPPLIED HEADER CARDS FOLLOW.  '        M37. 726
      FILE6(N11)(51:68) = ANSI32(1:18)    ! ADD ON DATE AND TIME        M37. 727
      DO 2708  J=1, N8   ! LOOP OVER ALL DECLARATION, NOW STORED AT TOP M37. 728
      N11 = N11 - 1     ! NEXT DESTINATION ADDRESS FOR DECLARATION CARD M37. 729
 2708 FILE6(N11) = FILE6(J)    ! TRANSFER DECLARATION FROM TOP TO BOTTOMM37. 730
      IF ( IPRSPY .LT. 1 )  GO TO 2721  ! JUMP AROUND DIAGNOSTIC        M37. 731
      WRITE (MUNIT6, 2716)  N8, N11, LIMCRD                             M37. 732
 2716 FORMAT ( ' DONE PROCESSING DECLARATIONS.  N8, N11,',              M37. 733
     1         ' LIMCRD =',  3I5  )                                     M37. 734
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 735
      WRITE (MUNIT6, 2717)                                              M37. 736
 2717 FORMAT ( '     ROW  KOLINC  KKKDUM  MODARG  ARGINC ....' )        M37. 737
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 738
      DO 2719  J=1, NUMARG  ! PRINT EACH ROW J OF ARGUMENT TABLE        M37. 739
      WRITE (MUNIT6, 2718)  J, KOLINC(J), KKKDUM(J),                    M37. 740
     1                         MODARG(J), ARGINC(J)                     M37. 741
 2718 FORMAT ( 4I8, A20 )                                               M37. 742
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 743
 2719 CONTINUE   ! END  DO 2719  LOOP OVER ALL ROWS "J" OF TABLE        M37. 744
 2721 N20 = 0        ! INITIALIZE NUMBER OF ARGUMENT USAGES FOUND SO FARM37. 745
      N16 = LIMCRD + 1     ! INITIALIZE LOCATION OF NEXT CARD CONSIDEREDM37. 746
      N13 = 0               ! SO FAR, NO ACTIVE (NON-COMMENT) CARDS READM37. 747
      DO 2766  N17 = N11, LIMCRD       ! PROCESS ALL NON-DECLATION CARDSM37. 748
      N16 = N16 - 1    ! REVERSE-INDEXED LOCATION OF NEXT NON-DECL. CARDM37. 749
      BUFF77 = FILE6(N16)      ! TRANSFER CARD TO SCALAR WORKING STORAGEM37. 750
      IF ( BUFF77(1:1) .EQ. 'C' )  GO TO 2766       ! SKIP COMMENT CARDSM37. 751
      N13 = N13 + 1   ! PRESENT CARD HAS THIS ACTIVE CARD NUMBER IN FILEM37. 752
      IF ( BUFF77(1:1) .EQ. '/' )  GO TO 2766    ! SKIP SORTING COMMANDSM37. 753
      IF ( BUFF77(1:4) .EQ. '$EOF' )  GO TO 2772    ! EFFECTIVE FILE ENDM37. 754
      DO 2754  J=1, NUMARG        ! CHECK PRESENT CARD FOR EACH ARGUMENTM37. 755
      L = 1              ! BEGIN SEARCH FOR STRING IN COLUMN 1 OF BUFF77M37. 756
      N15 = KOLINC(J)        ! NUMBER OF CHARACTERS IN THE J-TH ARGUMENTM37. 757
 2724 K = INDEX ( BUFF77(L:), ARGINC(J)(1:N15) )                        M37. 758
      IF ( K .EQ. 0 )  GO TO 2754            ! NO MORE J-TH STRING USAGEM37. 759
      N20 = N20 + 1      ! ANOTHER ARGUMENT USAGE FOUND; ADVANCE STORAGEM37. 760
      IF ( N20 .LE. 200 )  GO TO 2737     ! NOT OVERFLOW WORKING VECTORSM37. 761
      WRITE (MUNIT6, 2731)  N16                                         M37. 762
 2731 FORMAT ( '  ====   OVERFLOW ERROR STOP AT CARD NUMBER',           M37. 763
     1     I6,      '    OVER 200 ARGUMENTS.'  )                        M37. 764
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 765
      CALL STOPTP   ! INSTALLATION-DEPENDENT PROGRAM STOP CARD          M37. 766
 2737 KARG(N20) = J           ! IT IS THE J-TH STRING WHICH IS USED HEREM37. 767
      IF ( J .GT. NUMARG )    ! IF THIS IS A DUMMY RATHER THAN REAL ONE,M37. 768
     1 KARG(N20) = -(J-NUMARG)    ! STORE DUMMY ARG NUMBER WITH "-" FLAGM37. 769
      KARD(N20) = N13                 ! ACTIVE CARD COUNT OF THIS RECORDM37. 770
      KBEG(N20) = L - 1 + K   ! BEGINNING CARD COLUMN NUMBER FOR STRING M37. 771
      KEND(N20) = KBEG(N20) + N15 - 1     ! ENDING COLUMN NUMBER        M37. 772
      IF ( J .LE. NUMARG )       ! IF THIS IS A NON-DUMMY ARGUMENT, THENM37. 773
     1 KTEX(N20) = MODARG(J)        ! STORE RIGHT ADJUST FLAG (0=Y, 1=N)M37. 774
      L = KEND(N20) + 1  ! SEARCH CONTINUES ONE BYTE BEYOND STRING END  M37. 775
      IF ( IPRSPY .LT. 6 )  GO TO 2749  ! JUMP AROUND DIAGNOSTIC        M37. 776
      WRITE (MUNIT6, 2748)  N16, J, L, K, N20                           M37. 777
 2748 FORMAT ( '    ANOTHER STRING FOUND.  N16, J, L, K, N20 =', 5I6 )  M37. 778
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 779
 2749 GO TO 2724    ! LOOP BACK TO CHECK FOR ANOTHER APPEARANCE TO RIGHTM37. 780
 2754 CONTINUE         ! END  DO 2754  LOOP OVER EACH CANDIDATE STRING JM37. 781
      IF ( IPRSPY .LT. 2 )  GO TO 2766  ! JUMP AROUND DIAGNOSTIC        M37. 782
      WRITE (MUNIT6, 2759)  N16, N20, BUFF77                            M37. 783
 2759 FORMAT ( ' DONE WITH THIS CARD.  N16, N20 =', 2I5,                M37. 784
     1         '   BUFF77=',  A80 )                                     M37. 785
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 786
 2766 CONTINUE               ! END  DO 2766  LOOP OVER CARD NUMBER N16  M37. 787
 2772 IF ( IPRSPY .LT. 1 )  GO TO 2778  ! JUMP AROUND DIAGNOSTIC        M37. 788
      WRITE (MUNIT6, 2777)  N20                                         M37. 789
 2777 FORMAT ( ' DONE WITH IDENTIFYING ALL ARGUMENTS OF ALL CARDS.',    M37. 790
     1         '   N20 =',  I4 )                                        M37. 791
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 792
 2778 IF ( (N20/25)*25 .NE. N20 )  GO TO 2783   ! 25I3 CARDS NOT FILLED M37. 793
C     EXTRA ZERO ENTRY MUST BE ADDED, SINCE OTHERWISE CARD IS FULL:     M37. 794
      N20 = N20 + 1     ! ADD ZERO ENTRY, WHICH GOES ON BLANK EXTRA CARDM37. 795
      KARD(N20) = 0            ! ONLY VALUE ACTUALLY USED, OF QUINTUPLETM37. 796
      KARG(N20) = 0      ! REMOVE POSSIBLE GARBAGE, TO AVOID OUTPUT MESSM37. 797
      KBEG(N20) = 0      ! REMOVE POSSIBLE GARBAGE, TO AVOID OUTPUT MESSM37. 798
      KEND(N20) = 0      ! REMOVE POSSIBLE GARBAGE, TO AVOID OUTPUT MESSM37. 799
      KTEX(N20) = 0      ! REMOVE POSSIBLE GARBAGE, TO AVOID OUTPUT MESSM37. 800
 2783 WRITE (PROM80, 2787)                                              M37. 801
 2787 FORMAT ( ' SEND OUTPUT FILE NAME FOR FINAL $INCLUDE FILE :' )     M37. 802
      CALL PROMPT                                                       M37. 803
      READ (MUNIT5, 1329)  BUFF77                                       M37. 804
      OPEN ( UNIT=LUNT13, STATUS='NEW', FILE=BUFF77 )                   M37. 805
      REWIND LUNT13                                                     M37. 806
      ANSI8(1:4) = 'KARD'                                               M37. 807
      WRITE (LUNT13, 2791)  ANSI8(1:4),  ( KARD(J), J=1, N20 )          M37. 808
      ANSI8(1:4) = 'KARG'                                               M37. 809
      WRITE (LUNT13, 2791)  ANSI8(1:4),  ( KARG(J), J=1, N20 )          M37. 810
      ANSI8(1:4) = 'KBEG'                                               M37. 811
      WRITE (LUNT13, 2791)  ANSI8(1:4),  ( KBEG(J), J=1, N20 )          M37. 812
      ANSI8(1:4) = 'KEND'                                               M37. 813
      WRITE (LUNT13, 2791)  ANSI8(1:4),  ( KEND(J), J=1, N20 )          M37. 814
      ANSI8(1:4) = 'KTEX'                                               M37. 815
      WRITE (LUNT13, 2791)  ANSI8(1:4),  ( KTEX(J), J=1, N20 )          M37. 816
 2791 FORMAT ( A4, 25I3 ,/, (4X, 25I3) )                                M37. 817
      DO 2795  J=N11, LIMCRD           ! NEXT, DUMP ALL INPUT DATA CARDSM37. 818
 2795 WRITE (LUNT13, 2802)  FILE6 ( LIMCRD + N11 - J )                  M37. 819
 2802 FORMAT ( A80 )                                                    M37. 820
      CLOSE (UNIT=LUNT13)                                               M37. 821
      GO TO 1311   ! BACK TO ORIGINAL PROMPT AT START OF EMTP EXECUTION M37. 822
 9200 NCHAIN = 51  ! HEAD FOR ERROR OVERLAYS, FOR "KILL" MESSAGE        M37. 823
      LSTAT(18) = -1   ! OVERLAY NUMBER PRESENTLY BEING EXECUTED        M37. 824
 9800 IF ( NTACS .NE. 2 )  GO TO 9002                                   M42.7153
      K = 1    ! BEGIN TO CONVERT TACS DATA WITHOUT '/' CARDS           M42.7154
      DO 3500  J = 1, NUMCRD                                            M42.7155
      IF ( FILE6(J)(1:5) .NE. 'BLANK'                                   M42.7156
     1  .AND. FILE6(J)(1:80) .NE. BLAN80(1:80) )   GO TO 3005           M42.7157
      K = K + 1    ! COUNTER FOR TACS DATA TYPES                        M42.7158
      IF ( K .GE. 6 )  GO TO 9002  ! DONE WITH ALL 5 DATA TYPES         M42.7159
      FILE6(J)(1:2) = 'C '  !CONVERT BLANK CARD TO COMMENT CARD         M42.7160
      GO TO 3500            ! EXCEPT THE LAST ONE TO END TACS DATA      M42.7161
 3005 GO TO (3010, 3020, 3500, 3040, 3050), K                           M42.7162
 3010 IF (FILE6(J)(1:2) .EQ. '99')   !CONVERT TYPE CODE '99' TO         M42.7163
     1    FILE6(J)(1:2) = '  '       !'  ' FOR FUNCTIONS                M42.7164
      GO TO 3500                                                        M42.7165
 3020 IF ( FILE6(J)(1:1) .EQ. 'C' .OR. FILE6(J)(1:1) .EQ. '9'           M42.7166
     1   .OR. FILE6(J)(1:1) .EQ. '$') GO TO 3500                        M42.7167
      IF ( FILE6(J)(2:2) .NE. '1' )  GO TO 3022                         M42.7168
      FILE6(J)(1:1) = '1'     ! TO CONVERT TACS SOURCE CARDS:           M42.7169
      GO TO 3500              ! TYPE ' 1' TO '11', ' 2' TO '14'         M42.7170
 3022 IF ( FILE6(J)(2:2) .NE. '2' )  GO TO 3024                         M42.7171
      FILE6(J)(1:2)  = '14'   ! ' 3' TO '23', AND ' 4' TO '24'          M42.7172
      GO TO 3500                                                        M42.7173
 3024 FILE6(J)(1:1)  = '2'                                              M42.7174
      GO TO 3500                                                        M42.7175
 3040 IF (FILE6(J)(1:1) .NE. ' ')  GO TO 3500                           M42.7176
      FILE6(J)(1:2) = '33'        !CONVERT TYPE '  ' TO '33'            M42.7177
      GO TO 3500                  !FOR TACS OUTPUT VARIABLES            M42.7178
 3050 IF (FILE6(J)(1:1) .NE. ' ' )  GO TO 3500                          M42.7179
      FILE6(J)(1:2) = '77'          !CONVERT TYPE '  ' TO '77'          M42.7180
 3500 CONTINUE                 !FOR TACS INITIAL CONDITION CARDS        M42.7181
 9002 IF ( IPRSUP .LT. 1 )  GO TO 9007  ! JUMP AROUND DIAGNOSTIC        M42.7182
      WRITE (MUNIT6, 9004)  NUMCRD, LIMCRD, KILL                        M37. 826
 9004 FORMAT ( ' EXIT "DATAIN".   NUMCRD, LIMCRD, KILL =', 3I8 )        M37. 827
      CALL WINDOW         ! OUTPUT OF CHARACTER VARIABLE MUNIT6         M37. 828
 9007 RETURN                                                            M37. 829
      END                                                               M11.  78
