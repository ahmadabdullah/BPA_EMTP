      SUBROUTINE OVER15
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'TACSAR.FOR;/NOLIST'                                           
      INCLUDE  'SYNCOM.FOR;/NOLIST'                                           
      INCLUDE  'SYNMAC.FOR;/NOLIST'                                           
      INCLUDE  'UMDECK.FOR;/NOLIST'                                           
      INCLUDE  'SPACE2.FOR;/NOLIST'                                           
      DIMENSION   ISPUM(1)                                              M28.4662
      EQUIVALENCE          (D2, SM )                                    M29.1998
      EQUIVALENCE    ( SPUM(1), ISPUM(1) )                              M28.4663
      EQUIVALENCE  (MONCAR(1),    KNT),   (MONCAR(2),  KBASE)           M37.4951
      EQUIVALENCE  (MONCAR(3), LTDELT),   (MONCAR(4),    ISW)           M37.4952
      EQUIVALENCE  (MONCAR(5),  IDIST),   (MONCAR(6),  ITEST)           M37.4953
      REAL*8           AUPPER, ALOWER, TEXT1, TEXT2, TEXT3              M35.1410
      REAL*8           TEXT4, TEXT5, TEXT6, TEXT7                       M35.1411
      REAL*8           TEXT8, TEXT9, TEXT10, TEXT11, TEXT12             M37.4954
      CHARACTER*132  OUTLIN                                             M42.6640
      DIMENSION AUPPER(13), ALOWER(13)                                     12052
      DIMENSION  NSUBKM(1)                                              M42.6641
      EQUIVALENCE  ( KKNONL(1), NSUBKM(1) )                             M42.6642
      DATA  TEXT1   / 6HTACS   /                                        M16.2814
      DATA  TEXT4   / 6HNORMAL /                                        M35.1413
      DATA  TEXT5   / 6H       /                                        M35.1414
      DATA  TEXT6   / 6HUNIFOR /                                        M35.1415
      DATA  TEXT7   / 6HM      /                                        M35.1416
      DATA  TEXT8   / 6H MIDDL /                                        M35.1417
      DATA  TEXT9   / 6HE      /                                        M35.1418
      DATA  TEXT10  / 6HMINIMU /                                        M35.1419
      DATA  TEXT11  / 6HM      /                                        M35.1420
      DATA  TEXT12  / 6HCHAN01 /                                                
C     TRANSFER TO  "TOP15"  FOR FRONT END OF OVERLAY 15.                M28.4664
      IF ( IPRSUP  .GE.  1 )                                            M32.4697
     1 WRITE ( LUNIT6, 4567 )                                           M32.4698
 4567 FORMAT (24H  "BEGIN MODULE OVER15." )                             M32.4699
      CALL MOVE0 ( KSSFRQ(1), NTOT )                                    M32.4700
      CALL TOP15                                                        M33.1147
      IF ( KILL  .GT.  0 )   GO TO 9200                                 M33.1148
      GO TO 3038                                                        M33.1149
C     READ USER-SUPPLIED NAMES FOR EMTP NODE-VOLTAGE OUTPUT.  ----------M33.1150
C     READ INPUT CARD USING CIMAGE.                                     M33.1151
 1030 CALL CIMAGE                                                       M33.1152
 3038 CONTINUE                                                          M33.1153
      DECODE( 80, 3043, ABUFF)  IJK, AUPPER                             M34. 150
 3043 FORMAT( I2, 13A6 )                                                M33.1155
      IF ( IJK  .NE.  1 )   GO TO 8211                                  M33.1156
      NUMNVO = NTOT1                                                    M33.1157
      IVOLT = IJK                                                       M33.1158
      IF ( NOUTPR  .EQ.  0 )                                            M33.1159
     1 WRITE (KUNIT6, 3045)                                             M35.1421
 3045 FORMAT( 41H+REQUEST FOR OUTPUT OF ALL NODE VOLTAGES.   )          M33.1161
      CALL INTERP                                                       M35.1422
      IF ( AUPPER(1)  .EQ.  BLANK )  GO TO 3094                         M33.1162
      GO TO 1030                                                        M33.1163
 8211 L = 0                                                             M33.1164
      IF ( AUPPER(1) .NE. TEXT12 ) GO TO 4693                                   
C     HONOR "CHAN01" IN COLS. 3-8 BY BUILDING DUMMY NODE VOLTAGE        M37.4957
C     CHANNELS WITH NAMES "CHAN01", "CHAN02", ETC.:                     M37.4958
      DECODE (80, 4671, ABUFF) N13                                              
 4671 FORMAT ( 8X, I8 )                                                         
      IF ( N13 + NTOT .LE. LBUS   .AND.                                         
     1     N13 .GT. 0 )  GO TO 4682                                             
 4675 WRITE (LUNIT6, 4676)  N13, NTOT, LBUS                             M37.4963
 4676 FORMAT ( '   = = =  No, too many dummy node voltage',             M37.4964
     1         ' output channels for EMTP dimensions.'                  M37.4965
     2   ,/,   '          N13 + NTOT .GT. LBUS,   where   N13,',        M37.4966
     3         ' NTOT, LBUS =',  3I8,   '     If execution is'          M37.4967
     4   ,/,   '          interactive,  DEPOSIT  revised  N13',         M37.4968
     5         '  value in  ISTEP  at next SPY break.'  )               M37.4969
      CALL SPYING                                                               
      N13 = ISTEP                                                               
      ISTEP = 0                                                                 
 4682 IF ( NOUTPR .EQ. 0 )                                              M37.4973
     1 WRITE (KUNIT6, 4683)  N13                                                
 4683 FORMAT ( 33H+DUMMY NODE VOLTAGE OUTPUT NUMBER,  I8  )             M37.4975
      DO 4687  J=1, N13                                                         
      CALL NMINCR ( TEXT12, J )                                                 
      BUS(NTOT+J) = TEXT12                                                      
      NUMNVO = NUMNVO + 1                                                       
      IF ( NUMNVO .GT. LSIZ12 )  GO TO 4675                                     
      E(NTOT+J) = 0.0                                                            
 4687 IBSOUT(NUMNVO) = NTOT + J                                                 
      GO TO 1030                                                                
CCCCC  4693 DO 8288 I = 1, 13                                                 M37.4984
 4693 IF ( IJK .NE. -5 ) GO TO 1830 
CCCC  write (*,*) ' Begin search for branch voltages.   NV =', nv 
      DO 4962  I=1, 11, 2  
      IF ( AUPPER(I) .EQ. BLANK  .AND. 
     1     AUPPER(I+1) .EQ. BLANK ) GO TO 4962  
      DO 5183  JK=1, 2  
      BUS1 = AUPPER(I+JK-1)
CCCC  write (*,*) ' Ready to check next name.  JK, BUS1 =',  JK, BUS1
      DO 2801  IP=1, NTOT  
      IF ( BUS1 .NE. BUS(IP) ) GO TO 2801 
CCCC  write (*,*) ' Match found.   JK, IP =',  JK, IP 
      IF ( JK .EQ. 1 )  
     1 IBRNCH(NV+1) = IP
      IF ( JK .EQ. 2 )  
     1 JBRNCH(NV+1) = IP
      IF ( JK .EQ. 1 ) GO TO 5183
      NV = NV + 1 
CCCC  write (*,*) ' Done assigning V-branch out.   NV, IBRNCH(NV),', 
CCCC 1            ' JBRNCH(NV) =',   NV, IBRNCH(NV), JBRNCH(NV)
      GO TO 4962  
 2801 CONTINUE 
      IF ( NOUTPR  .EQ.  0 )  
     1 WRITE(LUNIT6, 6925 )  BUS1
 6925 FORMAT( 5X,  'Ignore V-branch request involving nonexistent',  
     1             ' node  "',  A6,   1H"   )
      GO TO 4962  
 5183 CONTINUE 
 4962 CONTINUE 
      IF ( NOUTPR  .EQ.  0 )  
     1 WRITE (KUNIT6, 3872)
 3872 FORMAT( '+CARD of name pairs for branch voltages.'   )
      GO TO 1030  
 1830 DO 8288  I=1, 13  
      IF ( IJK  .LT.  0 )  GO TO 8215                                   M33.1166
      BUS1 = AUPPER(I)                                                  M33.1167
      IF( BUS1 .EQ. BLANK )  GO TO 8288                                 M33.1168
      L = 1                                                             M33.1169
      DO 3052  IK = 2, NTOT                                             M33.1170
      IF( BUS1 .EQ. BUS(IK) )  GO TO 3056                               M33.1171
 3052 CONTINUE                                                          M33.1172
      IF ( NOUTPR  .EQ.  0 )                                            M33.1173
     1 WRITE(LUNIT6, 3053 )  BUS1                                       M33.1174
 3053 FORMAT( 5X,  48HREQUEST FOR VOLTAGE OUTPUT OF NONEXISTENT NODE ', M33.1175
     1 A6,   18H' WILL BE IGNORED.   )                                  M33.1176
      GO TO 8288                                                        M33.1177
 3056 IF ( IVOLT  .EQ.  1 )  GO TO 8888                                 M33.1178
      NUMNVO = NUMNVO + 1                                               M33.1179
      IBSOUT(NUMNVO) = IK                                               M33.1180
      IF ( NUMNVO  .LE.  LSIZ12 )   GO TO 8288                          M38.2938
      IPRINT = 11                                                       M33.1182
      LSTAT(19) = 3056                                                  M33.1183
      GO TO 9000                                                        M33.1184
 8888 WRITE ( LUNIT6, 8899 )                                            M33.1185
 8899 FORMAT ( 48H THERE HAS BEEN THE REQUEST FOR ALL NODE VOLTAGE,     M39.2704
     1         41H OUTPUT, SO THIS REQUEST WILL BE IGNORED. )           M33.1187
      GO TO 1030                                                        M33.1188
 8215 L = 1                                                             M33.1189
      IF ( AUPPER(I)  .EQ.  BLANK )  GO TO 8288                         M33.1190
      DO 8222 J = 1, IBR                                                M33.1191
      IP = NAMEBR(J)                                                            
      IF ( TEXVEC(IP) .EQ. AUPPER(I) ) GO TO 8231                               
 8222 CONTINUE                                                          M33.1193
      DO 8225  J = 1, INONL                                             M33.1194
      IP = NAMENL(J)                                                            
      IF ( TEXVEC(IP) .EQ. AUPPER(I) ) GO TO 8233                               
 8225 CONTINUE                                                          M33.1196
      DO 8227 J = 1, KSWTCH                                             M33.1197
      IP = NAMESW(J)                                                            
      IF ( TEXVEC(IP)  .EQ.  AUPPER(I) )  GO TO 8235                    M42.6650
 8227 CONTINUE                                                          M33.1199
      WRITE ( LUNIT6, 8229 )                                            M33.1200
 8229 FORMAT ( 45H     REQUEST FOR BRANCH OUTPUT OF NONEXISTENT,        M39.2705
     1         23H BRANCH WILL BE IGNORED )                             M33.1202
      GO TO 8288                                                        M33.1203
 8311 IPRINT = 11                                                       M33.1204
      LSTAT(19) = 8237                                                  M33.1205
      GO TO 9000                                                        M33.1206
 8333 IPRINT = 18                                                       M33.1207
      LSTAT(19) = 8237                                                  M33.1208
      GO TO 9000                                                        M33.1209
 8231 JK = 1111                                                         M33.1210
      NK1 = IABS ( KBUS(J) )                                            M33.1211
      NK2 = IABS ( MBUS(J) )                                            M33.1212
      IF ( IJK  .NE.  -2 )  MBUS(J) = - NK2                             M33.1213
      GO TO 8237                                                        M33.1214
 8235 JK = 3333                                                         M33.1215
      NK1 = KMSWIT(J)                                                   M33.1216
      NK2 = KMSWIT( J + LSWTCH )                                        M33.1217
      IF ( IJK  .NE.  -2 )  KPOS(J) = - IABS( KPOS(J) )                 M33.1218
      GO TO 8237                                                        M33.1219
 8233 JK = 2222                                                         M33.1220
      NK1 = NONLK(J)                                                    M33.1221
      NK2 = IABS( NONLM(J) )                                            M33.1222
      IF ( IJK  .NE.  -2 )  NONLM(J) = - NK2                            M33.1223
 8237 IF ( IJK  .GT.  -2 )  GO  TO  8288                                M33.1224
      NV = NV + 1                                                       M33.1225
      IF ( NV  .GT.  LSIZ12 )  GO TO 8311                               M38.2939
      IF ( IJK  .GE.  -3 )  GO TO 8260                                  M33.1227
      NPOWER = NPOWER + 1                                               M33.1228
      IF ( NPOWER  .GT.  MAXPE )  GO TO 8333                            M33.1229
      MPOWER = NPOWER + MAXPE                                           M35.1423
      KOUTVP(NPOWER) = NV                                               M33.1230
      IF ( JK  .EQ.  3333 )  KOUTVP(NPOWER) = - NV                      M33.1231
      KOUTVP(MPOWER) = J                                                M35.1424
      IF ( JK  .EQ. 2222 )  KOUTVP(MPOWER) = -J                         M35.1425
      IF ( JK .EQ. 3333  .AND.  KSWPE4 .EQ. 0 )                         M33.1234
     1 KSWPE4 = NPOWER                                                  M33.1235
      IF ( IPRSUP  .GT.  2 )                                            M33.1236
     1 WRITE(LUNIT6, 8244) NPOWER, MAXPE, KOUTVP(NPOWER),               M33.1237
     2                     KOUTVP(MPOWER)                               M35.1426
 8244 FORMAT ( /, 21H POWER OUTPUT REQUEST, 4I10 )                      M33.1239
 8260 IF ( JK .NE. 2222  .OR.  NK1 .LE. NK2 )  GO TO 8262               M33.1240
      IF ( IABS( NLTYPE(J) )  .NE.  99 )  GO TO 8262                    M33.1241
      IBRNCH(NV) = NK2                                                  M33.1242
      JBRNCH(NV) = NK1                                                  M33.1243
      GO TO 8288                                                        M33.1244
 8262 IBRNCH(NV) = NK1                                                  M33.1245
      JBRNCH(NV) = NK2                                                  M33.1246
 8288 CONTINUE                                                          M33.1247
      IF( L .EQ. 0 )  GO TO 3081                                        M33.1248
      IF ( NOUTPR  .EQ.  0 )                                            M33.1249
     1 WRITE (KUNIT6, 3072)                                             M35.1427
 3072 FORMAT( 43H+CARD OF BUS NAMES FOR NODE-VOLTAGE OUTPUT.   )        M33.1251
      GO TO 1030                                                        M33.1252
 3081 IF ( NOUTPR  .EQ.  0 )                                            M33.1253
     1 WRITE (KUNIT6, 54167)                                            M35.1428
54167 FORMAT(  49H+BLANK CARD ENDING NODE NAMES FOR VOLTAGE OUTPUT.    )M33.1255
      CALL INTERP                                                       M35.1429
      IF ( NENERG .EQ. 0 )  GO TO 3094                                  M33.1256
      WRITE (LUNIT6, 637)  NENERG                                       M33.1257
  637 FORMAT ( //,  87H THE DATA CASE NOW READY TO BE SOLVED IS A STATISM33.1258
     1TICAL OVERVOLTAGE STUDY WHICH INVOLVES, I7, 16H   ENERGIZATIONS ) M35.1430
      IF ( NENERG  .LT.  0 )   GO TO 1329                               M33.1262
      TEXT2 = TEXT4                                                     M35.1431
      TEXT3 = TEXT5                                                     M35.1432
      IF ( IDIST  .EQ.  0 )   GO TO 1310                                M35.1433
      TEXT2 = TEXT6                                                     M35.1434
      TEXT3 = TEXT7                                                     M35.1435
 1310 WRITE (LUNIT6, 1311)   TEXT2, TEXT3                               M35.1436
 1311 FORMAT ( 39H (PARAMETER  "NENERG"  OF COLUMNS 65-72,              M35.1437
     1         31H OF THE 2ND MISC. DATA CARD).  ,                      M35.1438
     2         40H SWITCH CLOSING OR OPENING TIMES FOR THE              M35.1439
     1   ,/,   46H SPECIALLY-FLAGGED SWITCHES (WITH "STATISTICS",       M35.1440
     2        46H PUNCHED IN COLUMNS 55-64) ARE VARIED RANDOMLY,        M35.1441
     3        16H ACCORDING TO A ,  A6, A1, 14H DISTRIBUTION. )         M35.1442
      WRITE (LUNIT6, 1324)                                              M33.1273
 1324 FORMAT (113H THE USER CAN SELECT EITHER UNIFORM OR NORMAL DISTRIBUM33.1274
     1TIONS, BASED ON THE VALUE OF PARAMETER  'IDIST'  OF COLUMNS  ,/,  M33.1275
     2 114H 17-24 OF THE SPECIAL STATISTICS MISC. DATA CARD.   THE FOLLOM33.1276
     3WING IS A LISTING OF SWITCHES WHOSE CLOSING TIMES ARE   ,/,       M33.1277
     4  117H TO BE STATISTICALLY VARIED, ALONG WITH THE ASSOCIATED MEAN M33.1278
     5AND STANDARD DEVIATION FOR THE DISTRIBUTION, AS REQUESTED   ,     M35.1443
     6   13H BY THE USER.      )                                        M33.1280
      IF ( XMAXMX  .LT.  0.0 )                                          M33.1281
     1 WRITE (LUNIT6, 1325)  XMAXMX                                     M33.1282
 1325 FORMAT (                                                          M33.1283
     1   47H THE USER PUNCHED STATISTICS MISCELLANEOUS DATA,            M33.1284
     2   20H PARAMETER  XMAXMX =,  E12.3,                               M33.1285
     3   41H ,   WITH THE NEGATIVE VALUE REPRESENTING   ,/,             M33.1286
     4   45H A REQUEST FOR USE OF THE 100 BUILT-IN RANDOM,              M33.1287
     5   41H NUMBERS.   AS SUCH, THE ANSWER IS REALLY,                  M33.1288
     6   39H DETERMINISTIC, THEN (A SECOND SOLUTION     ,/,             M33.1289
     7   46H ON ANY COMPUTER SHOULD GIVE THE SAME ANSWER).   )          M33.1290
      GO TO 1336                                                        M33.1291
 1329 TEXT2 = TEXT8                                                     M35.1444
      TEXT3 = TEXT9                                                     M35.1445
      IF ( ITEST  .GT.  0 )   GO TO 8329                                M35.1446
      TEXT2 = TEXT10                                                    M35.1447
      TEXT3 = TEXT11                                                    M35.1448
 8329 WRITE (LUNIT6, 1330)  TEXT2, TEXT3                                M35.1449
 1330 FORMAT ( 39H (PARAMETER  "NENERG"  OF COLUMNS 65-72,              M35.1450
     1         29H OF THE 2ND MISC. DATA CARD).           ,/,           M35.1451
     2         107H CLOSING TIMES FOR THE SPECIALLY-FLAGGED SWITCHES (WIM35.1452
     1TH  'SYSTEMATIC'  PUNCHED IN COLUMNS 55-64) ARE VARIED   ,/,  1X, M33.1294
     2 113HREGULARLY BETWEEN MINIMUM AND MAXIMUM TIMES, WITH A UNIFORM IM33.1295
     3NCREMENT IN BETWEEN FOR EACH SWITCH.   THE FOLLOWING        ,/,1X,M33.1296
     4 117HIS A LISTING OF THE SWITCHES WHOSE CLOSING TIMES ARE TO BE SYM33.1297
     5STEMATICALLY VARIED FOR THIS DATA CASE.   ALSO TABULATED   ,/, 1X,M33.1298
     6  19HARE THE ASSOCIATED ,  A6, A1,                                M35.1453
     6  48H  CLOSING TIMES AND STEP-INCREMENT SIZES FOR THE,            M35.1454
     7 53H SWITCHES, EXACTLY AS WAS READ FROM THE SWITCH CARDS.  )      M35.1455
 1336 WRITE (LUNIT6, 1338)                                              M33.1307
 1338 FORMAT ( /, 10X, 5HENTRY, 4X, 6HSWITCH, 6X, 4HFROM, 8X,           M33.1308
     1 2HTO, 9X, 13HCOLUMNS 15-24, 8X, 13HCOLUMNS 25-34,                M33.1309
     2 7X, 20HREFERENCE SWITCH NO., /,                                  M33.1310
     3 5X,  2(4X, 6HNUMBER),  2(7X, 3HBUS),  10X, 12H(IN SECONDS),      M33.1311
     4 9X,  12H(IN SECONDS),  5X,  24H(0 IMPLIES INDEPENDENCE)   )      M33.1312
      N1 = 0                                                            M33.1313
      DO 1352  I=1, KSWTCH                                              M33.1314
      IF ( ABSZ(ADELAY(I))  .NE.   44444. )   GO TO 1352                M33.1315
      L = IABS( KMSWIT(I) )                                             M33.1316
      NDX1 = LSWTCH + I                                                 M33.1317
      M = IABS( KMSWIT(NDX1) )                                          M33.1318
      N1 = N1 + 1                                                       M33.1319
      D1 = ABSZ( TOPEN(NDX1) )                                          M33.1320
      WRITE (LUNIT6, 1346)  N1, I, BUS(L), BUS(M), CRIT(I), D1,         M33.1321
     1                      KDEPSW(I)                                   M33.1322
 1346 FORMAT ( 5X, 2I10,  2(4X, A6), 2F21.6, 6X, I10 )                  M33.1323
 1352 CONTINUE                                                          M33.1324
      IF ( NENERG  .LT.  0 )   GO TO 1373                               M33.1325
      WRITE (LUNIT6, 1371)  DEGMIN, DEGMAX, STATFR                      M33.1326
 1371 FORMAT ( 109H NOW IN ADDITION TO SWITCH-TIME VARIATION CAUSED BY EM33.1327
     1ACH SWITCH'S OWN DISTRIBUTION, THERE IS THE ADDED RANDOM   ,/,    M33.1328
     2  108H DELAY WHICH IS THE SAME FOR ALL SWITCHES, REFERED TO BY THEM33.1329
     3 TERM 'REFERENCE ANGLE'.   DISTRIBUTION FOR THIS   ,/,            M33.1330
     4   45H ANGLE IS UNIFORM OVER THE TIME INTERVAL FROM,    E14.5,    M33.1331
     5 3H TO,    E14.5,  17H DEGREES BASED ON,  E13.3,  3H HZ   ,/,     M33.1332
     6  115H FREQUENCY.   THIS WAS ALL SPECIFIED BY THE USER USING FIELDM33.1333
     7S  'DEGMIN' ,  'DEGMAX' ,  AND  'STATFR'  (COLS. 41-64)   ,/,     M33.1334
     8   43H OF THE SPECIAL STATISTICS MISC. DATA CARD.     )           M33.1335
      WRITE(LUNIT6,1372)                                                M33.1336
 1372 FORMAT( 111H HOWEVER, IF THE PARAMETER 'ITEST' (FOUND IN COL. 9-16M33.1337
     1 OF THE STATISTICS MISC. DATA CARD) IS GREATER THAN ZERO,  /,     M33.1338
     2  87H THE REFERENCE ANGLE IS SET TO ZERO AND, CONSEQUENTLY, THERE M33.1339
     3IS NO ADDED RANDOM DELAY.  , /,1X)                                M33.1340
 1373 WRITE (LUNIT6, 1374)                                              M33.1341
 1374 FORMAT ( 109H IN THE FOLLOWING PRINTOUT, SWITCH TIMES WHICH ARE USM33.1342
     1ED FOR EACH ENERGIZATION ARE TABULATED, FOLLOWED BY PEAK   ,/,    M33.1343
     2  111H OUTPUT-VARIABLE VALUES WHICH OCCUR FOR THE ASSOCIATED SIMULM33.1344
     3ATION.   FORMAT FOR PEAK VALUES IS IDENTICAL TO THE   ,/,         M33.1345
     4   42H PRINTOUT FOR MAXIMUM VARIABLE VALUES OF A,                 M35.1456
     5   19H CONVENTIONAL STUDY     )                                   M35.1457
      IF ( NENERG  .GT.  0 )                                            M35.1458
     1 WRITE (LUNIT6, 1362)                                             M35.1459
 1362 FORMAT ( 28H EXCEPT THAT PRINTOUT OF THE,                         M39.2706
     1         27H REFERENCE ANGLE IN DEGREES,                          M35.1461
     2         36H HAS BEEN ADDED AT THE EXTREME LEFT.    )             M35.1462
 3094 NC = NV                                                           M33.1355
      IF( IPRSUP .LT. 3 )  GO TO 7436                                   M33.1356
      WRITE(LUNIT6, 7427)                                               M33.1357
 7427 FORMAT ( /, 101H MIXTURE OF INTEGER ARRAYS IN OVER15.   ONLY THE LM33.1358
     1ONGEST COLUMN(S) WILL NOT HAVE GARBAGE AT THE ENDS.   ,/,         M33.1359
     2 12X, 3HROW, 5X, 5HBUS1 , 6X, 4HBUS2, 4X, 6HKSWTYP,               M33.1360
     3 6X, 4HKPOS, 6X, 4HNODE, 4X, 6HISOURC,                            M33.1361
     4 6X, 4HKBUS, 6X, 4HMBUS, 5X, 5HNONLK, 5X, 5HNONLM   )             M33.1362
      N2 = KSWTCH                                                       M33.1363
      IF( KCONST .GT. N2 )  N2 = KCONST                                 M33.1364
      IF( IBR .GT. N2 )  N2 = IBR                                       M33.1365
      IF( INONL .GT. N2 )  N2 = INONL                                   M33.1366
      DO 7431  K=1, N2                                                  M33.1367
      NDX1 = LSWTCH + K                                                 M33.1368
 7431 WRITE (LUNIT6, 7432) K,KMSWIT(K),KMSWIT(NDX1),KSWTYP(K),          M33.1369
     1 KPOS(K), NODE(K), ISOURC(K), KBUS(K), MBUS(K),                   M33.1370
     2 NONLK(K), NONLM(K)                                               M33.1371
 7432 FORMAT ( 5X , 11I10 )                                             M33.1372
 7436 IPRINT = 11                                                       M33.1373
      IF( IPRSUP .GE. 2 )                                               M33.1374
     1 WRITE(LUNIT6, 8025)  IBR, INONL, KSWTCH, NV, NPOWER              M33.1375
 8025 FORMAT( /, 9H AT 8025   , 5I10 )                                  M33.1376
      IF(KSWTCH.EQ.0) GO TO 912                                         M33.1377
      IF( IPRSUP .GE. 2 )                                               M35.1463
     1 WRITE (LUNIT6, 8026)  ( KOUTVP(L), L=1, NPOWER )                 M35.1464
      NDX1 = MAXPE + 1                                                  M35.1465
      NDX2 = MAXPE + NPOWER                                             M35.1466
      IF ( IPRSUP  .GE.  2 )                                            M35.1467
     1 WRITE (LUNIT6, 8026)  ( KOUTVP(L), L=NDX1, NDX2 )                M35.1468
 8026 FORMAT( /, 17H KOUTVP OR KOUTIE   ,/, ( 1X, 10I10 ) )             M35.1469
      DO 907 K=1,KSWTCH                                                 M33.1381
      N3 = KPOS(K)                                                      M33.1382
      IF( N3  .GE.  0 )  GO TO 907                                      M33.1383
      NC=NC+1                                                           M33.1384
      LSTAT(19)=  907                                                   M33.1385
      IF( NC .GT. LSIZ12 )  GO TO 9000                                  M38.2940
      N2=IABS(KMSWIT(K))                                                M33.1387
      IBRNCH(NC) = N2                                                   M33.1388
      NDX2 = LSWTCH + K                                                 M33.1389
      JBRNCH(NC) = IABS( KMSWIT(NDX2) )                                 M33.1390
      IF ( IPRSUP  .GE.  4 )  WRITE(LUNIT6, 8028)  K, NC, N3, N2        M33.1391
 8028 FORMAT ( /, 9H AT 8028 , 4I10 )                                   M33.1392
      DO 5438 I = 1, NPOWER                                             M33.1393
      MPOWER = MAXPE + I                                                M35.1470
      IF ( KOUTVP(I)  .GT.  0 )  GO TO 5438                             M33.1394
      IF ( KOUTVP(MPOWER)  .EQ.  K )  GO TO 4385                        M35.1471
 5438 CONTINUE                                                          M33.1396
      GO TO 907                                                         M33.1397
 4385 KOUTVP(MPOWER) = - NC                                             M35.1472
  907 CONTINUE                                                          M33.1399
  912 IF ( INONL  .EQ.  0 )  GO TO 8411                                 M33.1400
      DO 909 K = 1, INONL                                               M33.1401
      ICHECK=NONLM(K)                                                   M33.1402
      IF (IPRSUP  .GT.  3)  WRITE(LUNIT6, 8029)  K, ICHECK, NC          M33.1403
 8029 FORMAT( /, 9H AT 8029  , 3I10 )                                   M33.1404
      IF(ICHECK.GE.0) GO TO 909                                         M33.1405
      NC=NC+1                                                           M33.1406
      LSTAT(19) = 8029                                                  M33.1407
      IF( NC .GT. LSIZ12 )  GO TO 9000                                  M38.2941
      IBRNCH(NC) = NONLK(K)                                             M33.1409
      JBRNCH(NC) = - ICHECK                                             M33.1410
      DO 5439 I = 1, NPOWER                                             M33.1411
      MPOWER = MAXPE + I                                                M35.1473
      IF ( KOUTVP(I) .LT. 0 ) GO TO 5439                                VAX. 118
      IF ( KOUTVP(MPOWER)  .GE.  0 )  GO TO 5439                        M35.1474
      IF ( KOUTVP(MPOWER)  .EQ.  -K ) GO TO 4398                        M35.1475
 5439 CONTINUE                                                          M33.1414
      GO TO 909                                                         M33.1415
 4398 KOUTVP(MPOWER) = - ( 1000 + NC )                                  VAX. 119
  909 CONTINUE                                                          M33.1417
C     BRING "N15" OF "TOP15" OUT INTO "OVER15" USING "NFRFLD"           M28.4667
 8411 N15 = NFRFLD                                                      M33.1418
      IF ( NUMUM  .GT.  0 )                                             M35.1477
     1 CALL UMINIT (  N15, SPUM(IUREAC),                                M33.1419
     1 SPUM(IUGPAR), SPUM(IUFPAR), SPUM(IUHIST), SPUM(IUUMRP),          M33.1420
     2 ISPUM(IUNOD1), ISPUM(IUNOD2), ISPUM(IUJCLT), ISPUM(IUJCLO),      M32.4701
     3 ISPUM(IUJTYP), ISPUM(IUNODO), ISPUM(IUJTMT), SPUM(IUHISM),       M32.4702
     4 SPUM(IUOMGM), SPUM(IUOMLD), SPUM(IUTHAM), SPUM(IUREDU),          M27.1292
     5 SPUM(IUREDS), SPUM(IUFLDS), SPUM(IUFLDR), SPUM(IUREQU),          M27.1293
     6 SPUM(IUFLQS), SPUM(IUFLQR), ISPUM(IUJCDS), ISPUM(IUJCQS),        M32.4703
     7 SPUM(IUFLXD), SPUM(IUFLXQ), ISPUM(IUNPPA), SPUM(IUROTM),         M32.4704
     8 ISPUM(IUNCLD), ISPUM(IUNCLQ), ISPUM(IUJTQO), ISPUM(IUJOMO),      M32.4705
     9 ISPUM(IUJTHO), SPUM(IUREQS), SPUM(IUEPSO), SPUM(IUDCOE),         M32.4706
     9 ISPUM(IUKCOI), SPUM(IUVOLT), SPUM(IUANGL), ISPUM(IUNODF),        M32.4707
     9 ISPUM(IUNODM), ISPUM(IUKUMO), ISPUM(IUJUMO), SPUM(IUUMOU))       M32.4708
      IF ( KILL .GT. 0 ) GO TO 9200                                     M37.4985
      IF ( IPRSUP  .GE.  1 )                                            M27.1300
     1 WRITE (LUNIT6, 7309)  NUMSUB, N15, NCOMP                         M25. 543
 7309 FORMAT ( /,  28H DONE SETTING COMP. VECTORS.,                     M25. 544
     1             24H  NUMSUB     N15   NCOMP  ,/,  28X,  3I8  )       M25. 545
      IF ( IPRSUP  .LT.  2 )   GO TO 7321                               M25. 546
      N11 = NUMSUB + 1                                                  M25. 547
      WRITE (LUNIT6, 7312)  ( ISUBEG(K), K=1, N11 )                     M25. 548
 7312 FORMAT (  9H ISUBEG :,  10I10   )                                 M25. 549
      N12 = ISUBEG(NUMSUB) + 4                                          M42.6651
      WRITE (LUNIT6, 7315) ( NSUBKM(K), K=1, N12)                               
 7315 FORMAT ( 9H NSUBKM :,  10I10  )                                   M42.6653
      WRITE (LUNIT6, 7319)  ( KUPL(K), K=1, INONL )                     M25. 554
 7319 FORMAT (  7H KUPL :,  10I10   )                                   M25. 555
      WRITE (LUNIT6, 7320)  ( VECNL2(K), K=1, INONL )                   M25. 556
 7320 FORMAT (  9H VECNL2 :,  10E12.4  )                                M25. 557
 7321 N14 = LCOMP * LBUS                                                M25. 558
      N13 = NCOMP * NTOT                                                M25. 559
      IF ( N13  .LE.  N14 )   GO TO 7327                                M25. 560
      KILL = 1                                                          M25. 561
      IPRINT = 24                                                       M25. 562
      LSTAT(19) = 7321                                                  M25. 563
      GO TO 9000                                                        M25. 564
 7327 DO 911  K=1, IBR                                                  M33.1421
      ICHECK=MBUS(K)                                                       12232
      IF (IPRSUP .GE. 3)  WRITE(LUNIT6, 8030)  K, ICHECK, NC            M33.1422
 8030 FORMAT ( 37H AT 8030 OF "OVER15", K, ICHECK, NC =, 3I6 )          M33.1423
      IF( ICHECK .GE. 0 )  GO TO 911                                    M33.1424
      NC=NC+1                                                              12237
      LSTAT(19) = 8030                                                  M18. 793
      IF( NC .GT. LSIZ12 )  GO TO 9000                                  M38.2942
      IBRNCH(NC) = IABS( KBUS(K) )                                         12241
      JBRNCH(NC) = - ICHECK                                             M33.1425
      DO 5540 I = 1, NPOWER                                             M33.1426
      MPOWER = MAXPE + I                                                M35.1478
      IF ( KOUTVP(I)  .LE.  0 )  GO TO 5540                             M33.1427
      IF ( KOUTVP(MPOWER)  .EQ.  K )  GO TO 5415                        M35.1479
 5540 CONTINUE                                                          M33.1429
      GO TO 911                                                         M33.1430
 5415 KOUTVP(MPOWER) = - NC                                             M35.1480
  911 CONTINUE                                                             12258
      DO 5507 I = 1, NPOWER                                             M33.1432
      MPR = MAXPE + I                                                   M35.1481
      IF ( KOUTVP(MPR) .GE. 0 )  GO TO 5507                             VAX. 120
      N44 = IABS ( KOUTVP(MPR) )                                        VAX. 121
      IF ( N44 .GT. 1000 ) N44 = N44 - 1000                             VAX. 122
      KOUTVP(MPR) = N44                                                 VAX. 123
 5507 CONTINUE                                                          VAX. 124
      LSTAT(31) = NC                                                       12260
      IF ( KTAB  .GT.  0 )   GO TO 4752                                 M33.1434
      IOUTCS = 0                                                        M22.4690
 4752 IF( NUMSM .GT. 0  )    CALL SMOUT                                         
      LSTAT( 37 ) = NST                                                         
      LSTAT( 36 ) = MFIRST                                                      
      IF( KILL .GT. 0 )    GO   TO   9200                               M38.2946
      N5 = NC + NSMOUT + IOUTCS + NUMOUT                                M38.2947
      N16 = 1                                                           M27.1302
      IF ( NUMBUS  .GT.  0 )   N16 = NUMBUS                             M27.1303
      N9 = IOUTCS + 1                                                   M38.2948
      N23 = 0                                                           M38.2949
      CALL NAMEA6( TEXT1, N23 )                                         M38.2950
      NTACS = NTOT + N23                                                M38.2951
      N8 = NTOT + MAXBUS                                                        
      N18 = N8 + N16                                                    M27.1304
      IF ( IPRSUP  .GE.  1 )                                            M16.2826
     1 WRITE (LUNIT6, 3207)  NC, LSIZ12, NSMOUT, IOUTCS,                M38.2953
     2                   NTOT, NUMNVO, NV, N8, MAXBUS, N9, N18          M38.2954
 3207 FORMAT ( /,   32H BEFORE OUTPUT-VARIABLE HEADING.,                M16.2828
     1  40H      NC  LSIZ12  NSMOUT  IOUTCS    NTOT,                    M38.2955
     2  48H  NUMNVO      NV      N8  MAXBUS      N9     N18             M38.2956
     3  ,/,  32X,  12I8  )                                              M27.1307
      IF ( N5  .LE.  LSIZ12 )   GO TO 3204                              M38.2957
      KILL = 179                                                        M16.2832
      LSTAT(19) = 3204                                                  M38.2958
      LSTAT(14) = N5                                                    M16.2834
      LSTAT(15) = NSMOUT                                                M16.2835
      LSTAT(16) = IOUTCS                                                M16.2836
      LSTAT(13) = NUMOUT                                                M27.1308
      LSTAT(12) = NC                                                    M42.6654
      GO TO 9200                                                        M16.2837
 3204 NCSAVE = NC                                                       M16.2839
      IF (NENERG .NE. 0  .AND.  KBASE .NE. INTINF)  GO TO 3224          M32.4810
      I = 0                                                             M16.2840
      I3 = 0                                                                    
 3206 I = I + 1                                                         M16.2841
      IF ( I .GT. NSMOUT ) GO TO 3209                                   M16.2842
      NC = NC + 1                                                       M16.2843
      I3 = I3 + 3                                                               
      IBRNCH( NC ) = ISMOUT( I3-1 ) + NTOT                                      
      JBRNCH( NC ) = ISMOUT(  I3  ) + NTOT                                      
      GO TO 3206                                                        M16.2846
 3209 I = 0                                                             M16.2847
 3211 I = I + 1                                                         M16.2848
      IF ( I  .GT.  IOUTCS )   GO TO 3218                               M16.2849
      NC = NC + 1                                                       M16.2850
      IBRNCH(NC) = NTACS                                                M38.2963
      IK = JOUT( KJOUT + I )                                            M38.2964
      JBRNCH(NC) = NTOT + ILNTAB( KLNTAB + IK )                         M38.2965
      GO TO 3211                                                        M16.2853
 3218 IF ( NUMOUT  .LE.  0 )   GO TO 3224                               M27.1310
      N17 = IUKUMO - 1                                                  M32.4811
      N19 = IUJUMO - 1                                                  M32.4812
      DO 3221  I=1, NUMOUT                                              M27.1314
      NC = NC + 1                                                       M27.1315
      NDX1 = N17 + I                                                    M27.1316
      NDX2 = N19 + I                                                    M27.1317
      IBRNCH(NC) = ISPUM(NDX1) + N8                                     M27.1318
 3221 JBRNCH(NC) = ISPUM(NDX2) + N8                                     M27.1319
 3224 NUMBCO = NC - NV                                                  M27.1320
      LSTAT(31) = NC                                                    M19.1608
      K = NUMNVO + NC                                                      12428
      N2 = LSIZ26                                                       M31.3083
      IF (IAVERG .EQ. 0) N2 = N2 + N2                                   M22.4696
      IF( K .LE. N2 )  GO TO 3098                                          12431
      KILL = 1                                                          M32.4813
      LSTAT(16) = 26                                                    M32.4814
      LSTAT(19) = 3098                                                  M32.4815
      GO TO 9200                                                           12438
 3098 LSTAT(32) = K                                                        12439
      IF ( BEGMAX(1) .LE. 0.0 ) GO TO 1061                              M36.1480
      IF( K .LE. LSIZ12 )  GO TO 1063                                   M38.2966
      IPRINT = 12                                                       M12. 506
      LSTAT(19) = 1063                                                  M12. 507
      GO TO 9000                                                           12445
C     NEXT INITIALIZE EXTREMA VECTOR  "XMAX" .   THERE ARE 4            M27.1321
C     PARTITIONS OF SIZE  LIST 12:  (XMAX TMAX XMIN TMIN) .             M27.1322
 1063 DO 1064  I=1, K                                                   M27.1323
      XMAX(I) = -FLTINF                                                 M27.1324
      NDX1 = LSIZ12 + I                                                 M38.2967
      XMAX(NDX1) = 0.0                                                  M27.1326
      NDX1 = LSIZ12 + NDX1                                              M38.2968
      XMAX(NDX1) = FLTINF                                               M27.1328
      NDX1 = LSIZ12 + NDX1                                              M38.2969
 1064 XMAX(NDX1) = 0.0                                                  M27.1330
 1061 CALL RUNTYM(D1,D2)                                                M20.4148
      LIMSTP = KPRCHG(INDSTP)                                           M20.4149
      FLSTAT(7) = FLSTAT(7) - D1                                        M20.4150
      FLSTAT(8) = FLSTAT(8) - D2                                        M20.4151
      IF (IOUT .GE. 0) GO TO 300                                        M20.4152
      ISPRIN = INTINF                                                   M20.4153
      LIMSTP = INTINF                                                   M20.4154
  300 IF (IPLOT .LT. 0) ISPLOT = INTINF                                 M20.4155
      LUNIT6SAVE = LUNIT6                                               M43.   5
      LUNIT6 = 79                                                       M43.   6
      OPEN (79, STATUS='SCRATCH')                                       M43.   7
      IF (NENERG .NE. 0  .AND.  KBASE .NE. INTINF) GO TO 54242          M36.1481
      FLSTAT(5) = FLSTAT(5) + D1                                        M20.4172
      FLSTAT(6) = FLSTAT(6) + D2                                        M20.4173
      IF (IPLOT .LT. 0 .AND.  KBASE .EQ. 0 ) GO TO 7802                 M32.4817
      IF ( IPLOT .LT. 0 )  GO TO 3106                                   M32.4818
      REWIND LUNIT4                                                     M29.2001
      WRITE (LUNIT4)  DATE1, TCLOCK,                                    M29.2002
     1        N18, NUMNVO, NUMBCO, NC,  ( BUS(I), I=1, NTOT),           M27.1332
     2                   ( TEXVEC(I), I = 1, MAXBUS ),                  M38.2970
     3                   ( BUSUM(I), I=1, N16 )                         M38.2971
      IF ( IPRSUP  .GE.  1 )                                            M20.4175
     1 WRITE (*, 3207)  NC, LSIZ12, NSMOUT, IOUTCS,                     M43.   9
     2                   NTOT, NUMNVO, NV, N8, MAXBUS, N9, N18          M38.2973
      IF ( IVOLT  .NE.  1 )   GO TO 3103                                   12450
      WRITE (LUNIT4)   ( I, I=2, NTOT )                                 M29.2003
      GO TO 3104                                                           12452
 3103 IF( NUMNVO .GT. 0 )                                                  12453
     1 WRITE (LUNIT4)  ( IBSOUT(I), I=1, NUMNVO )                       M29.2004
 3104 IF( NC .GT. 0 )                                                      12455
     1 WRITE (LUNIT4)  (IBRNCH(I), I = 1, NC), (JBRNCH(I), I = 1, NC)   M29.2005
 3106 IF ( KBASE .NE. 1  .AND.  KBASE .NE. INTINF )  GO TO 7802         M32.4819
      WRITE (LUNIT9)  DATE1, TCLOCK, IVOLT, MAXBUS, NDX1, NDX2, N16,    M38.2974
     1     NSMOUT, IOUTCS, NUMOUT, N18, NUMNVO, NUMBCO, NC, KSWTCH      M32.4821
      WRITE (LUNIT9)  (BUS(I), I=1, NTOT),                              M32.4822
     1       ( TEXVEC( I ), I = 1, MAXBUS ),                            M38.2975
     3       ( BUSUM(I), I=1, N16 )                                     M38.2976
      IF ( IPRSUP  .GE.  1 )                                            M32.4825
     1 WRITE (*, 3207)  NC, LSIZ12, NSMOUT, IOUTCS,                     M43.  10
     2                   NTOT, NUMNVO, NV, N8, MAXBUS, N9, N18          M38.2978
      IF ( IVOLT  .NE.  1 )   GO TO 4103                                M32.4828
      WRITE (LUNIT9)   ( I, I=2, NTOT )                                 M32.4829
      GO TO 4104                                                        M32.4830
 4103 IF( NUMNVO .GT. 0 )                                               M32.4831
     1 WRITE (LUNIT9)  ( IBSOUT(I), I=1, NUMNVO )                       M32.4832
 4104 IF( NC .GT. 0 )                                                   M32.4833
     1 WRITE (LUNIT9)  (IBRNCH(I), I = 1, NC), (JBRNCH(I), I = 1, NC)   M32.4834
 7802 IF ( IVOLT  .EQ.  1 )   GO TO 3107                                M32.4835
      IF ( NUMNVO  .GT.  NC )   LSTAT(31) = NUMNVO                         12458
 3107 IF ( NOUTPR  .NE.  0 )   GO TO 54242                              M20.4178
      N4 = NCSAVE - NV                                                  M32.4836
      IF ( KOL132  .EQ.  132 )   GO TO 6637                             M32.4837
      WRITE (LUNIT6, 6632)  NUMNVO, NV, N4, NSMOUT, IOUTCS              M32.4838
 6632 FORMAT ( 39H TIME-STEP LOOP BEGINS.  NUMBER OF NODE,              M32.4839
     1         42H VOLTAGES, BRANCH VOLTAGES, CURRENTS, TACS            M32.4840
     2   ,/,   36H VARIABLES, AND S.M. VARIABLES ARE :,  5I6  )         M32.4841
      GO TO 6645                                                        M32.4842
 6637 WRITE (LUNIT6, 3108)  K                                           M32.4843
 3108 FORMAT ( /, 24H COLUMN HEADINGS FOR THE,  I4,  73H  EMTP OUTPUT VAM16.2859
     1RIABLES FOLLOW.   THESE ARE ORDERED ACCORDING TO THE FIVE     ,/, M16.2860
     2   55H POSSIBLE EMTP OUTPUT-VARIABLE CLASSES, AS FOLLOWS ....   ) M16.2861
      WRITE (LUNIT6, 3114)  NUMNVO                                      M17. 813
 3114 FORMAT ( 3X,  5HFIRST,  I4,  85H  OUTPUT VARIABLES ARE ELECTRIC-NEM16.2863
     1TWORK NODE VOLTAGES (WITH RESPECT TO LOCAL GROUND)|        )      M16.2864
      WRITE (LUNIT6, 3115)  NV                                          M17. 814
 3115 FORMAT ( 3X,  5H NEXT,  I4,  91H  OUTPUT VARIABLES ARE BRANCH VOLTM16.2866
     1AGES (VOLTAGE OF UPPER NODE MINUS VOLTAGE OF LOWER NODE)|    )    M16.2867
      WRITE (LUNIT6, 3116)  N4                                          M17. 815
 3116 FORMAT ( 3X,  5H NEXT,  I4,  87H  OUTPUT VARIABLES ARE BRANCH CURRM16.2870
     1ENTS (FLOWING FROM THE UPPER EMTP NODE TO THE LOWER)|        )    M16.2871
      WRITE (LUNIT6, 3117)  NSMOUT                                      M17. 816
 3117 FORMAT ( 3X,  5H NEXT,  I4,  92H  OUTPUT VARIABLES PERTAIN TO DYNAM16.2873
     1MIC SYNCHRONOUS MACHINES, WITH NAMES GENERATED INTERNALLY|     )  M16.2874
      WRITE (LUNIT6, 3118)  IOUTCS                                      M17. 817
 3118 FORMAT ( 3X,  5HFINAL,  I4,  81H  OUTPUT VARIABLES BELONG TO  'TACM16.2876
     1S'  (NOTE INTERNALLY-ADDED UPPER NAME OF PAIR).     )             M18. 795
      WRITE (LUNIT6, 3122)                                              M17. 818
 3122 FORMAT ( 105H BRANCH POWER  CONSUMPTION (POWER  FLOW, IF A SWITCH)M16.2879
     1 IS TREATED LIKE A BRANCH VOLTAGE FOR THIS GROUPING|         ,/,  M16.2880
     2         105H BRANCH ENERGY CONSUMPTION (ENERGY FLOW, IF A SWITCH)M16.2881
     3 IS TREATED LIKE A BRANCH CURRENT FOR THIS GROUPING.     )        M16.2882
 6645 IF( IPRSUP .LE. 0 )  GO TO 54242                                  M32.4844
      IF( NPOWER .LE. 0 )  GO TO 54242                                     12467
      WRITE(LUNIT6, 3112)  ( IBRNCH(I), JBRNCH(I), I=1, NC )            M17. 819
 3112 FORMAT( /,  72H 'IBRNCH'  AND  'JBRNCH'  PAIRS OF BRANCH-OUTPUT NO   12469
     1DE NUMERS, IN  OVER15    ,/,  ( 1X, 20I6 ) )                         12470
      WRITE(LUNIT6, 3113 )  ( KOUTVP(I), I=1, NPOWER )                  M35.1483
 3113 FORMAT( /,  32H 'KOUTVP'  AND  'KOUTIE'  PAIRS.    ,/,               12472
     1 ( 1X, 10I12 ) )                                                     12473
      NDX1 = MAXPE + 1                                                  M35.1484
      NDX2 = MAXPE + NPOWER                                             M35.1485
      WRITE(LUNIT6, 3113)  ( KOUTVP(L), L=NDX1, NDX2 )                  M35.1486
54242 K = 0                                                                12474
      N1 = 0                                                               12475
      IF (NENERG .NE. 0)   N1 = 1                                       M13.2474
      KPRSTA = 1                                                        M11. 267
      IF(NENERG.GT.0) KPRSTA=0                                             12477
      GO TO 3152                                                           12478
 3119 K = K + 1                                                            12479
      J = K + 1                                                            12480
      IF ( IVOLT  .NE.  1 )   J = IBSOUT(K)                                12481
      M = M + 1                                                            12482
      AUPPER(M) = BUS(J)                                                   12483
      ALOWER(M) = BLANK                                                    12484
      IF( M .LT. 9 )  GO TO 3158                                           12485
      ASSIGN  3152  TO MOON                                             M15.1162
 3126 IF ( NOUTPR  .NE.  0 )   GO TO 7113                               M20.4180
      IF ( N1  .EQ.  1 )                                                M35.1487
     1 WRITE (LUNIT6, 3131)  ( AUPPER(I), I=1, 9 )                      M35.1488
 3131 FORMAT ( /, 15X,  9(7X, A6)  )                                    M15.1164
      IF ( N1  .EQ.  0 )                                                M35.1489
     1 WRITE (LUNIT6, 3132)  ( AUPPER(I), I=1, 9 )                      M35.1490
 3132 FORMAT ( /,  15H  STEP     TIME,  9( 7X, A6 )  )                  M35.1491
      IF( N1 .GT. 0 )  GO TO 3144                                       M15.1165
      N1 = 1                                                            M15.1166
 3144  IF ( KPRSTA  .EQ.  1 )                                           M35.1492
     1 WRITE (LUNIT6, 3145)  ( ALOWER(I), I=1, 9 )                      M35.1493
 3145 FORMAT( 15X, 9(7X, A6) )                                          M15.1170
      IF ( KPRSTA  .EQ.  0 )                                            M35.1494
     1 WRITE (LUNIT6, 8145)  ( ALOWER(I), I=1, 9 )                      M35.1495
 8145 FORMAT( 15H  REF. ANGLE   ,  9( 7X, A6 )   )                      M35.1496
      KPRSTA=1                                                             12489
 7113 GO TO MOON,  ( 3152, 3166, 3188, 3197, 3177, 8637 )               M28.4677
 3152 M = 0                                                                12490
 3158 IF( K .LT. NUMNVO )  GO TO 3119                                      12491
      K = 0                                                                12492
      GO TO 3169                                                           12493
 3164 K = K + 1                                                            12494
      L = IBRNCH(K)                                                        12495
      J = JBRNCH(K)                                                        12496
      M = M + 1                                                            12497
      AUPPER(M) = BUS(L)                                                   12498
      IF( L .EQ. 1 )  AUPPER(M) = TERRA                                    12499
      ALOWER(M) = BUS(J)                                                   12500
      IF( J .EQ. 1 )  ALOWER(M) = TERRA                                    12501
      IF( M .LT. 9 )  GO TO 3169                                           12502
      ASSIGN  3166  TO MOON                                             M15.1172
      GO TO 3126                                                        M15.1173
 3166 M = 0                                                             M15.1174
 3169 IF ( K  .LT.  NCSAVE )   GO TO 3164                               M16.2884
      K = 0                                                             M16.2885
      IF ( NENERG  .NE.  0 )   GO TO 3198                               M23.3779
      GO TO 3191                                                        M16.2886
 3182 K = K + 1                                                         M16.2887
      L = ISMOUT( 3*K-1 )                                               M38.2979
      J = ISMOUT(  3*K  )                                               M38.2980
      M = M + 1                                                         M16.2890
      AUPPER( M ) = TEXVEC( L )                                         M38.2981
      ALOWER( M ) = TEXVEC( J )                                         M38.2982
      IF ( M  .LT.  9 )   GO TO 3191                                    M16.2893
      ASSIGN  3188  TO MOON                                             M16.2894
      GO TO 3126                                                        M16.2895
 3188 M = 0                                                             M16.2896
 3191 IF ( K  .LT.  NSMOUT )   GO TO 3182                               M16.2897
      K = 0                                                             M16.2898
      GO TO 3199                                                        M16.2899
 3195 K = K + 1                                                         M16.2900
      M = M + 1                                                         M16.2901
      AUPPER(M) = TEXT1                                                 M18. 796
      NDX1 = JOUT( KJOUT + K )                                          M38.2983
      NDX1 = ILNTAB( KLNTAB + NDX1 )                                    M38.2984
      ALOWER(M) = TEXVEC(NDX1)                                          M38.2985
      IF ( M  .LT.  9 )   GO TO 3199                                    M16.2904
      ASSIGN  3197  TO MOON                                             M16.2905
      GO TO 3126                                                        M16.2906
 3197 M = 0                                                             M16.2907
 3199 IF ( K  .LT.  IOUTCS )   GO TO 3195                               M16.2908
      K = 0                                                             M27.1335
      GO TO 8641                                                        M27.1336
 8634 K = K + 1                                                         M27.1337
      NDX1 = N17 + K                                                    M27.1338
      NDX2 = N19 + K                                                    M27.1339
      L = ISPUM(NDX1)                                                   M27.1340
      J = ISPUM(NDX2)                                                   M27.1341
      M = M + 1                                                         M27.1342
      AUPPER(M) = BUSUM(L)                                              M27.1343
      ALOWER(M) = BUSUM(J)                                              M27.1344
      IF ( M  .LT.  9 )   GO TO 8641                                    M27.1345
      ASSIGN 8637 TO MOON                                               M27.1346
      GO TO 3126                                                        M27.1347
 8637 M = 0                                                             M27.1348
 8641 IF ( K  .LT.  NUMOUT )   GO TO 8634                               M27.1349
 3198 IF ( M  .EQ.  0 )   GO TO 3177                                    M23.3780
 3171 M = M + 1                                                            12509
      AUPPER(M) = BLANK                                                    12510
      ALOWER(M) = BLANK                                                    12511
      IF( M .LT. 9 )  GO TO 3171                                           12512
      ASSIGN  3177  TO MOON                                             M15.1175
      GO TO 3126                                                        M15.1176
 3177 KCOUNT = NV                                                       M32.4845
      LUNIT6 = LUNIT6SAVE                                               M42.6658
      REWIND 79                                                         M42.6659
      DO 8936  IP=1, 99999                                              M42.6660
      READ (79, 8927, END=8644)  OUTLIN                                 M42.6661
 8927 FORMAT ( A132 )                                                   M42.6662
 8936 WRITE (LUNIT6, 8927)  OUTLIN                                      M42.6663
 8644 IF ( KSWTCH  .LE.  0 )   GO TO 8719                               M43.  12
      N11 = 0                                                           M32.4847
      DO 8713 I=1,KSWTCH                                                M32.4848
      NDX4 = LSWTCH + I                                                 M32.4849
      K = KMSWIT(I)                                                     M32.4850
      M = KMSWIT(NDX4)                                                  M32.4851
      D6 = TCLOSE(I)                                                    M32.4852
      IF ( KTRLSW(1)  .EQ.  8877 )   GO TO  8688                        M32.4853
      D6 = 0.0                                                          M32.4854
      IF ( TCLOSE(I)  .GT.  0.0  )   GO TO  8710                        M32.4855
      IF ( TCLOSE(I)  .EQ.  0.0 )   GO TO 8695                          M32.4856
      TCLOSE(I) = 0.0                                                   M32.4857
      GO TO 8689                                                        M32.4858
 8688 IF (NEXTSW(I) .NE. 87) GO TO 8695                                 M32.4859
 8689 D11 = 0.0                                                         M32.4860
      IF ( KBASE .EQ. 2 )  GO TO 8702                                   M39.2707
      WRITE (LUNIT6,8692)  TCLOSE(I),  BUS(K), BUS(M),  D11             M32.4861
 8692 FORMAT ( 27H ***          PHASOR I(0) =,  E15.7,  33X,            M32.4862
     1  8HSWITCH ",  A6,  6H" TO ",  A6,  14H" CLOSED AFTER,            M32.4863
     2  E12.5,  5H SEC.   )                                             M32.4864
      GO TO 8702                                                        M32.4865
 8693 D6 = 0.0                                                          M32.4866
      GO TO  8710                                                       M32.4867
 8695 IF (TCLOSE(I) .GT. 0.0 )  GO TO 8693                              M32.4868
      IF ( IABS( KPOS(I) ) .NE. 10 ) GO TO 8284                         M42.6664
      IF ( ABSZ( E(K) - E(M) ) .LT. TOPEN(I) ) GO TO 8710               M42.6665
 8284 IF ( KBASE .EQ. 2 )  GO TO 8702                                   M42.6666
      WRITE (LUNIT6, 8699 )  BUS(K), BUS(M),  TCLOSE(I)                 M32.4869
 8699 FORMAT ( 27H ***                       ,  15X,    33X,            M32.4870
     1  8HSWITCH ",  A6,  6H" TO ",  A6,  14H" CLOSED AFTER,            M32.4871
     2  E12.5,  5H SEC.   )                                             M32.4872
 8702 N8 = KPOS(I)                                                      M32.4873
      KPOS(I) = 2                                                       M32.4874
      IF ( N8 .LT. 0 )  KPOS(I) = -2                                    M32.4875
      N11 = N11 + 1                                                     M32.4876
      MODSWT(N11) = I                                                   M32.4877
 8710 IF ( KPOS(I)  .GE.  0 )   GO TO 8713                              M32.4878
      KCOUNT = KCOUNT + 1                                               M32.4879
      BVALUE(KCOUNT) = D6                                               M32.4880
      IF ( IPRSUP  .GE.  1 )                                            M32.4881
     1 WRITE (LUNIT6, 8712)  KCOUNT, BVALUE(KCOUNT)                     M32.4882
 8712 FORMAT ( 25H KCOUNT, BVALUE(KCOUNT) =,  I8,  E15.6 )              M32.4883
 8713 CONTINUE                                                          M32.4884
      KTRLSW(1) = N11                                                   M32.4885
 8719 NC = NCSAVE                                                       M32.4886
      IF (KBASE .NE. 2  .AND.  KBASE .NE. INTINF) GO TO 710             M32.4887
      IF ( ISW  .EQ.  0 )   GO TO 710                                   M32.4888
      N6 = 0                                                            M32.4890
      DO 4216  K1 =1, KSWTCH                                            M32.4891
      IF ( IABS( KPOS(K1) )  .EQ.  2 )   GO TO 4208                     M32.4892
      IF ( ADELAY(K1) .NE.  44444. )   GO TO 4216                       M33.1435
      VOLTI(N6+1) = TCLOSE(K1)                                          M32.4894
      GO TO 4213                                                        M32.4895
 4208 IF ( ADELAY(K1) .NE. -44444. )   GO TO 4216                       M33.1436
      VOLTI(N6+1) = TOPEN(K1)                                           M32.4897
 4213 N6 = N6 + 1                                                       M32.4898
      IF ( N6  .LT.  2*LSIZ26 )   GO TO 4215                            M32.4899
      LSTAT(19) = 4213                                                  M32.4900
      IPRINT = 26                                                       M32.4901
      GO TO 9200                                                        M32.4902
 4215 NEXTSW(N6) = K1                                                   M32.4903
 4216 CONTINUE                                                          M32.4904
      WRITE (LUNIT6, 701) KNT, (NEXTSW(K1), VOLTI(K1), K1=1, N6 )       M32.4905
  701 FORMAT (/, 32X, 46HRANDOM SWITCHING TIMES FOR ENERGIZATION NUMBER,M32.4906
     1 I4, /,  ( 32X,   5(I4, E16.6)   )   )                            M01.  93
  710 IF (IPRSUP .GE. 1)                                                M13.2476
     1 WRITE(LUNIT6, 1077)  KPRCHG, MULTPR                              M17. 826
 1077 FORMAT( /,  43H BEGIN DEL-T LOOP AT 1077.  KPRCHG, MULTPR=,          12523
     1 /, ( 1X, 12I10 ) )                                                  12524
      CALL MOVE0 ( NEXTSW(1), KSWTCH )                                  M32.4907
 1079 IF ( IPRSUP  .GE.  1 )                                            M19.1611
     1 WRITE (LUNIT6, 76101)  (I, IFORM(I), NODE(I), CREST(I), TIME1(I),M17. 827
     2  TIME2(I), TSTART(I), SFREQ(I), I = 1, KCONST)                   M13.2479
76101 FORMAT (/, 14H SOURCE TABLE.   , /,                               M13.2480
     1  5X, 3HROW, 3X, 5HIFORM, 4X, 4HNODE, 10X, 5HCREST,10X,5HTIME1,   M13.2481
     2  10X, 5HTIME2, 9X, 6HTSTART, 10X, 5HSFREQ, / , (3I8, 5E15.6))    M13.2482
      IF( IPRSUP .GT. 0 )                                                  12529
     1WRITE(LUNIT6, 73899)                                              M17. 828
73899 FORMAT( /, 45H N.L. ELEM TABLE UPON ENTERING TIME-STEP LOOP   ,/,    12531
     1 5X, 3HROW, 2X, 6HNLTYPE, 3X, 5HNONLK, 3X, 5HNONLM, 2X,              12532
     1 6HNONLAD, 2X, 6HNONLE , 9X, 6HVECNL1, 3X, 5HANONL,               M33.1437
     2 4X, 4HCURR, 8X, 5HVNONL  )                                       M33.1438
      I = 1                                                                12535
73900 IF( I .GT. INONL )  GO TO 73902                                      12536
      IF( IPRSUP .GT. 0 )                                                  12537
     1 WRITE(LUNIT6, 73901)  I, NLTYPE(I),  NONLK(I),  NONLM(I),        M17. 829
     1 NONLAD(I), NONLE(I), VECNL1(I), ANONL(I), CURR(I),               M33.1439
     2 VNONL(I)                                                         M33.1440
73901 FORMAT ( 6I8,  E15.5,  2F8.0,  E12.3  )                           M22.4707
      I = I + 1                                                            12542
      GO TO 73900                                                          12543
73902 I = 1                                                                12544
73903 IF( I .GT. ICHAR )  GO TO 1000                                       12545
      IF( IPRSUP .GT. 1 )                                                  12546
     1WRITE(LUNIT6, 73904)   I, GSLOPE(I), CCHAR(I), VCHAR(I)           M33.1441
73904 FORMAT(  30H N.L. CHAR AT 73904 OF MAIN14.   , I10, 3E15.5  )        12548
      I = I + 1                                                            12549
      GO TO 73903                                                          12550
 1000 IF ( NENERG  .NE.  0 )   GO TO 1009                               M23.3781
      N1 = 2 * MODOUT                                                   M11. 269
      IF ( MODOUT  .EQ.  0 )   GO TO 9999                               M25. 566
      IF ( NOUTPR  .EQ.  0 )                                            M20.4189
     1 WRITE (LUNIT6, 1006)  N1, MODOUT                                 M20.4190
 1006 FORMAT ( /, 10X,  45HREMEMBER ---- WHAT ARE LABELED AS THE INITIALM11. 272
     1, I3,  39H  BRANCH-OUTPUT CURRENTS ARE IN REALITY, /,             M35.1497
     2 24X,             100HMODAL VOLTAGES AT THE TWO ENDS OF THE LAST DM11. 274
     3ISTRIBUTED-PARAMETER LINE OF THE DATA CASE BEING SOLVED.    ,/,   M11. 275
     4 24X,               9HTHE FIRST, I3,  85H  MODE VOLTAGES AT THE  'M11. 276
     5BUS1'  END ALL COME FIRST, FOLLOWED BY ALL THE CORRESPONDING   ,/,M11. 277
     6 24X,     41HENTRIES FOR THE  'BUS2'  END OF THE LINE.,/,1X)      M15.1179
      GO TO 9999                                                        M25. 567
 1009 IF ( KBASE  .EQ.  INTINF )   KBASE = 2                            M32.4909
      IF ( KBASE  .EQ.  2     .AND.                                     M32.4910
     1     TENERG  .LT.  0.0 )                                          M25. 569
     2 CALL TABLES                                                      M25. 570
      LTDELT = 0                                                        M25. 571
      GO TO 9999                                                        M25. 572
 9000 LSTAT(16) = IPRINT                                                   12561
      KILL = 1                                                             12562
 9200 LASTOV = NCHAIN                                                   M23.3782
      NCHAIN = 51                                                          12564
      LSTAT(18) = 15                                                       12565
      IF ( IPRSUP  .GE.  1 )                                            M32.4911
     1 WRITE ( LUNIT6, 4568 )                                           M32.4912
 4568 FORMAT (24H  "EXIT  MODULE OVER15." )                             M32.4913
      GO TO 99999                                                       M20.4194
 9999 LASTOV = NCHAIN                                                   M20.4195
      NCHAIN = NCHAIN + 1                                               M20.4196
      IF ( IPRSUP  .GE.  1 )                                            M32.4914
     1 WRITE ( LUNIT6, 4568 )                                           M32.4915
99999 RETURN                                                               12566
      END                                                                  12567
      SUBROUTINE TOP15                                                  M28.4678
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      DIMENSION  NSUBKM(1)                                              M37.4987
      EQUIVALENCE  ( KKNONL(1), NSUBKM(1) )                             M37.4988
      EQUIVALENCE (LSTAT(14), KNUM)                                     M28.4687
      IF ( IPRSUP  .GE.  1 )                                            M28.4688
     1 WRITE (LUNIT6, 752)  NENERG, IBR, KSWTCH, INONL                  M32.4919
  752 FORMAT ( /,  17H ENTER  "TOP15" .,                                M28.4690
     1             32H  NENERG     IBR  KSWTCH   INONL                  M32.4920
     2  ,/,  17X,  5I8  )                                               M28.4692
      KNUM = 1                                                          M32.4921
      KNTBR=0                                                           M28.4703
      IPRSOV(35) = 0                                                    M39.2709
      KCOUNT=2                                                          M28.4706
      K=0                                                               M28.4707
      I = 1                                                             M28.4708
11905 IF (KBUS(I) .GE. 0)  GO TO 905                                    M28.4709
      CI1 = CI(I)                                                       M28.4710
C      IF (KODSEM(I) .NE. 0 .AND. IMODEL(I) .NE. -2)  GO TO 309         M31.3086
C 309  IF (KODSEM(I) .NE. 0 .AND. IMODEL(I) .NE. -4)  GO TO 903         M43.2246
      IF (KODSEM(I) .NE. 0 .AND. IMODEL(I) .GE. 0)  GO TO 903           M31.3086
      CK1 = ABSZ(CK(I))                                                 M28.4712
      A=1.0                                                             M28.4713
      IF(CI1.LT.0.) A=CK1                                               M28.4714
      IF( CK(I) .GE. 0.0 )  GO TO 25903                                 M28.4715
      L = CIK(I)                                                        M28.4716
      A = 1.0 / ETA(L)                                                  M28.4717
      IF( IPRSUP .GE. 2 )                                               M28.4718
     1WRITE(LUNIT6, 24903)  I, L, K, A, CK1, CI(I)                      M28.4719
24903 FORMAT( /, 26H FREQ DEP  2/M*Z  AT 24903   , 3I10, 3E20.6  )      M28.4720
25903 CI(I) = CI(I) * (A + CK1 )                                        M28.4721
  905 I = I + 1                                                         M28.4722
      GO TO 904                                                         M28.4723
  903 NN1 = IABSZ(KODEBR(I))                                            M28.4724
      I = I + NN1                                                       M28.4725
  904 IF (I .LE. IBR)  GO TO 11905                                      M28.4726
      IF ( KCONST  .EQ.  0 )   GO TO 912                                M33.1442
      DO 906 K=1,KCONST                                                 M28.4728
      IF ( IFORM(K) .EQ. 17 )                                           M32.4922
     1 CREST(K) = CREST(K+1)                                            M32.4923
      IF(IABS(IFORM(K)).NE.14) GO TO 902                                M28.4729
      SFREQ(K) = SFREQ(K) * TWOPI                                       M28.4730
      IF(TSTART(K).LT.0.) TSTART(K)=0.                                  M28.4731
  902 IF(IABS(IFORM(K)).NE.16) GO TO 906                                M28.4732
      CK1=TSTOP(K+1)                                                    M28.4733
      TSTOP(K+1)=0.                                                     M28.4734
      N3=IABS(NODE(K))                                                  M28.4735
      N2=IABS(NODE(K+1))                                                M28.4736
      GUS4=E(N3)-E(N2)                                                  M28.4737
      GUS3=CREST(K+2)-GUS4*TSTOP(K)                                     M28.4738
      GUS1=GUS3                                                         M28.4739
      IF(IFORM(K+1).GT.1) GUS1=CK1                                      M28.4740
      GUS2=GUS4*TSTOP(K)-CREST(K)                                       M28.4741
      CK1 = GUS2 / TIME1(K) + GUS1 * SFREQ(K) / CREST(K+1)              M28.4742
      H1 = TIME1(K+1) * TSTOP(K) - CREST(K)                             M28.4743
      H2=TSTART(K+1)*TSTOP(K)-CREST(K)                                  M28.4744
      CREST(K)=CREST(K)+2.0*CK1                                         M28.4745
      SFREQ(K)=1.0-2.0*SFREQ(K)/CREST(K+1)                              M28.4746
      TIME1(K) = 1.0 -2.0 / TIME1(K)                                    M28.4747
      CREST(K+1)=2.0*CK1                                                M28.4748
      SFREQ(K+1)=GUS2                                                   M28.4749
      TIME1(K+1) = H1                                                   M28.4750
      TIME2(K+1) = H2                                                   M28.4751
      TSTART(K+1) = SFREQ(K)*GUS3 + TIME1(K)*GUS2                       M28.4752
  906 CONTINUE                                                          M28.4753
  912 N15 = -4                                                          M37.4989
      NUMSUB = 0                                                        M28.4832
      IF(INONL.EQ.0) GO TO 9200                                         M33.1443
      CALL MOVER0 ( CURSUB(1), INONL )                                  M37.4990
      CALL MOVE ( KPSOUR(1), KSSFRQ(1), NTOT )                          M37.4991
 7209 DO 7248 K=1,INONL                                                 M31.3109
      NLSUB(K) = 0                                                      M28.4865
      IF ( NLTYPE(K)  .LT.  0 )   GO TO 7248                            M28.4866
      N15 = N15 + 5                                                     M37.4992
 7234 NSUBKM(N15) = N15                                                 M37.4993
      NSUBKM(N15+1) = NONLK(K)                                          M37.4994
      NSUBKM(N15+2) = IABS( NONLM(K) )                                  M37.4995
      NSUBKM(N15+3) = K                                                 M37.4996
      NSUBKM(N15+4) = 0                                                 M37.4997
      IF( NLTYPE(K) .GT. 920 )                                          M37.4998
     1 NSUBKM( N15+4 ) = NLTYPE( K ) - 920                              M37.4999
      MARK = KSSFRQ( NSUBKM(N15+1) )                                    M37.5000
      IF ( MARK .EQ. 0 ) MARK = KSSFRQ( NSUBKM(N15+2) )                 M37.5001
      IF ( MARK .LT. NTOT .AND. MARK .NE. 0) GO TO 2345                 M37.5002
      NUMSUB = NUMSUB + 1                                               M37.5003
      ISUBEG(NUMSUB) = N15                                              M37.5004
      NLSUB(K) = NUMSUB                                                 M37.5005
      IF ( MARK .EQ. 0 ) GO TO 6789                                     M37.5006
      DO 1234 IL = 2, KPARTB                                            M37.5007
 1234 IF ( KSSFRQ(IL) .EQ. MARK ) KSSFRQ(IL) = NUMSUB                   M37.5008
      GO TO 6789                                                        M37.5009
 7777 KILL = 229                                                        M37.5010
      LSTAT(15) = K                                                     M37.5011
      LSTAT(16) = NSUBKM(NN15+3)                                        M37.5012
      GO TO 9200                                                        M37.5013
 2345 NN15 = ISUBEG(MARK)                                               M37.5014
      IF ( NSUBKM( N15+4) .EQ. 0 )  GO TO 7777                          M37.5015
      IF ( NSUBKM(NN15+4) .EQ. 0 )  GO TO 7777                          M37.5016
      NUMC = 2                                                          M37.5017
 2745 NN16 = NSUBKM(NN15)                                               M37.5018
      IF ( NN15 .GE. NN16 ) GO TO 4567                                  M37.5019
      NN15 = NN16                                                       M37.5020
      NUMC = NUMC + 1                                                   M37.5021
      GO TO 2745                                                        M37.5022
 4567 NSUBKM(NN15) = N15                                                M37.5023
      NSUBKM(N15)  = NN16                                               M37.5024
      NLSUB(K) = MARK                                                   M37.5025
      IF ( NUMC  .GT. NCOMP ) NCOMP = NUMC                              M37.5026
 6789 IF ( IPRSUP  .GE.  1 )                                            M37.5027
     1 WRITE (LUNIT6, 7241)  K,  NUMSUB, N15, NCOMP, ISUBEG(NUMSUB)     M37.5028
 7241 FORMAT ( 41H  K, NUMSUB, N15, NCOMP, ISUBEG(NUMSUB) =,  5I8 )     M37.5029
 7248 CONTINUE                                                          M28.5010
      IF ( IPRSUP .GE. 7 )                                              M42.6667
     1 WRITE (*,*) ' TOP15.  KSSFRQ(1:NTOT) =',                         M42.6668
     2                     ( KSSFRQ(K), K=1, NTOT )                     M42.6669
      DO 7251  K=2, KPARTB                                              M42.6670
      IF ( KSSFRQ(K) .GT. NTOT )  KSSFRQ(K) = 0                         M42.6671
 7251 CONTINUE                                                          M42.6672
      IF ( IPRSUP .GE. 7 )                                              M42.6673
     1 WRITE (*,*) ' After tamper..  KSSFRQ(1:NTOT) =',                 M42.6674
     2                     ( KSSFRQ(K), K=1, NTOT )                     M42.6675
      CALL MOVE ( KSSFRQ(1), KPSOUR(1), NTOT )                          M42.6676
 9200 CALL MOVE0 ( KODE(1), NTOT )                                      M32.4935
      NFRFLD = N15                                                      M37.5031
      IF ( IPRSUP  .GE.  3 )                                            M32.4936
     1 WRITE (LUNIT6, 9201)                                             M28.5050
 9201 FORMAT (  16H EXIT  "TOP15" .   )                                 M28.5051
      RETURN                                                            M28.5053
      END                                                               M28.5054
      SUBROUTINE SMOUT                                                  M16. 922
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
C     THIS MODULE IS USED ONLY BY  TYPE 59 S.M.  MODELING               M38.2986
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'SYNMAC.FOR;/NOLIST'                                           
      REAL*8          TEXTA,TEXTB,DIGIT,TEXT1,TEXT2,BUSVEC              M38.2988
      DIMENSION  TEXTA(15),  DIGIT(10),  TEXTB(3),  BUSVEC(1)           M29.1506
      DATA TEXTA(1)   /6HID    /                                        M16. 927
      DATA TEXTA(2)   /6HIQ    /                                        M16. 928
      DATA TEXTA(3)   /6HI0    /                                        M16. 929
      DATA TEXTA(4)   /6HIF    /                                        M16. 930
      DATA TEXTA(5)   /6HIKD   /                                        M16. 931
      DATA TEXTA(6)   /6HIG    /                                        M16. 932
      DATA TEXTA(7)   /6HIKQ   /                                        M16. 933
      DATA TEXTA(8)   /6HIA    /                                        M33. 566
      DATA TEXTA(9)   /6HIB    /                                        M33. 567
      DATA TEXTA(10)  /6HIC    /                                        M33. 568
      DATA TEXTA(11)  /6HEFD   /                                        M33. 569
      DATA TEXTA(12)  /6HMFORCE/                                        M33. 570
      DATA TEXTA(13)  /6HMANG  /                                        M33. 571
      DATA TEXTA(14)  /6HTQ GEN/                                        M33. 572
      DATA TEXTA(15)  /6HTQ EXC/                                        M33. 573
      DATA  TEXTB(1)   /  6HANG     /                                   M20.2031
      DATA  TEXTB(2)   /  6HVEL     /                                   M20.2032
      DATA  TEXTB(3)   /  6HTOR     /                                   M20.2033
      DATA  DIGIT(1)    /  6H1       /                                  M20.2034
      DATA  DIGIT(2)    /  6H2       /                                  M20.2035
      DATA  DIGIT(3)    /  6H3       /                                  M20.2036
      DATA  DIGIT(4)    /  6H4       /                                  M20.2037
      DATA  DIGIT(5)    /  6H5       /                                  M20.2038
      DATA  DIGIT(6)    /  6H6       /                                  M20.2039
      DATA  DIGIT(7)    /  6H7       /                                  M20.2040
      DATA  DIGIT(8)    /  6H8       /                                  M20.2041
      DATA  DIGIT(9)    /  6H9       /                                  M20.2042
      DATA  DIGIT(10)   /  6H0       /                                  M20.2043
      DATA  TEXT1   /  6HMACH    /                                      M20.2044
      LL5 = 5                                                           M20.2047
      LL6 = 6                                                           M20.2048
      NSMOUT = 0                                                        M16. 997
      JK = 0                                                            M16. 998
      JK1 = 0                                                                   
      IF ( IPRSUP  .GE.  1 )                                            M18. 395
     1 WRITE (LUNIT6, 1328)  NST, LSYN                                  M38.2990
 1328 FORMAT ( /,  17H BEGIN  'SMOUT' .,                                M38.2991
     1 16H     NST    LSYN   ,/,   17X,  4I8  )                         M38.2992
      N1 = 0                                                            M20.2056
      N2 = 0                                                            M20.2057
      IP1 = -2                                                                  
      DO 3648  IP = 1, NST                                                      
      I30 = 30 * IP - 29                                                        
      N2 = N2 + 1                                                       M20.2059
      IF ( N2  .LE.  9 )   GO TO 4113                                   M20.2060
      N2 = 0                                                            M20.2061
      N1 = N1 + 1                                                       M20.2062
 4113 I = IP                                                            M38.2996
      N = I                                                             M16.1003
      I5 = I30 + 17                                                     M37.2890
      N15 = ISMDAT( I5 )                                                M37.2891
      IF( N15 .LT. 0 )  GO TO  3648                                     M33. 579
      L = 5                                                             M20.2065
      BUSVEC(1) = TEXT1                                                 M29.1507
      IF ( N1  .GT.  0 )                                                M38.2997
     1 CALL PACKA1 ( DIGIT(N1), BUSVEC(1), L )                          M29.1510
      L = L + 1                                                         M20.2077
      N10 = N2                                                          M20.2078
      IF ( N10  .EQ.  0 )   N10 = 10                                    M20.2079
      CALL PACKA1 ( DIGIT(N10), BUSVEC(1), L )                          M29.1511
      TEXT2 = BUSVEC( 1 )                                                       
      ICNT = 0                                                                  
      CALL NAMEA6( TEXT2, ICNT )                                                
      IF( N15 .EQ. 0 )   GO TO 21                                       M33. 580
      DO 4178 L = 1, N15                                                M33. 581
      IP1 = IP1 + 3                                                             
      M = ISMOUT( IP1 )                                                         
      JK = JK + 1                                                       M38.3003
      JK1 = JK1 + 3                                                     M38.3004
      ISMOUT( JK1-1 ) = ICNT                                                    
      TEXT2 = TEXTA( M )                                                        
      JB = 0                                                                    
      CALL NAMEA6( TEXT2, JB )                                                  
 4178 ISMOUT(  JK1  ) = JB                                                      
 21   DO 4224  N7 = 1, 3                                                M38.3010
      I5 = I5 + 1                                                       M33. 585
      N15 = ISMDAT( I5 )                                                M37.2892
      IF( N15 .EQ. 0 )  GO  TO  4224                                    M33. 587
      DO 4217 L = 1, N15                                                M33. 588
      IP1 = IP1 + 3                                                             
      N6 = ISMOUT( IP1 )                                                        
      N5 = N6 / 10                                                      M33. 591
      N6 = N6 - 10 * N5                                                 M36. 708
      BUSVEC(1) = TEXTB(N7)                                             M29.1513
      IF ( N5  .GT.  0 )   GO TO 4206                                   M20.2105
      CALL PACKA1 ( DIGIT(N6), BUSVEC(1), LL5 )                         M29.1514
      GO TO 4215                                                        M38.3013
 4206 CALL PACKA1 ( DIGIT(N5), BUSVEC(1), LL5 )                         M29.1515
      N10 = N6                                                          M20.2109
      IF ( N10  .EQ.  0 )   N10 = 10                                    M20.2110
      CALL PACKA1 ( DIGIT(N10), BUSVEC(1), LL6 )                        M29.1516
 4215 JK = JK + 1                                                       M38.3014
      JK1 = JK1 + 3                                                     M38.3015
      ISMOUT( JK1-1 ) = ICNT                                                    
      JB = 0                                                                    
      TEXT2 = BUSVEC( 1 )                                                       
      CALL NAMEA6( TEXT2, JB )                                                  
 4217  ISMOUT(  JK1  ) = JB                                                     
 4224 CONTINUE                                                          M20.2119
      IF ( JK  .LE.  LSMOUT )   GO TO 3648                                      
      KILL = 1                                                          M38.3022
      LSTAT( 19 ) = 4224                                                M38.3023
      LSTAT( 16 ) = 11                                                  M38.3024
      RETURN                                                            M19. 771
 3648 CONTINUE                                                          M38.3025
      IF ( IPRSUP  .GE.  3 )                                            M20.2129
     1 WRITE( LUNIT6, 4240 )  (I, ISMOUT(3*I), ISMOUT(3*I-1), I=1, JK)  M38.3026
 4240 FORMAT ( /,  35H (I, JSMOUT(I), KSMOUT(I), I=1, JK)    ,/,        M20.2131
     1 ( 1X,  10( 3I4,  1X ) )  )                                       M20.2132
      NSMOUT = JK                                                       M16.1100
      D12 = NBYTE( 4 )                                                          
      D12 = ( 3 * NSMOUT ) * D12 / NBYTE( 3 )                                   
      MSMOUT = D12 + 1.0                                                        
      RETURN                                                            M16.1101
      END                                                               M16.1102
      SUBROUTINE UMINIT (  N15,                                         M27.1350
     1 REACL, GPAR, FPAR, HIST,UMCURP, NODVO1, NODVO2, JCLTAC,          M33.1446
     2 JCLOUT,JTYPE,NODOM, JTMTAC, HISTOM, OMEGM, OMOLD, THETAM,        M33.1447
     3 REAMDU, REAMDS, FLXDS, FLXDR, REAMQU, FLXQS, FLXQR,              M27.1353
     4 JCDSAT, JCQSAT, FLXD, FLXQ, NPPAIR, ROTMOM, NCLD,                M27.1354
     5 NCLQ, JTQOUT, JOMOUT, JTHOUT, REAMQS, EPSOM, DCOEF,              M27.1355
     6 KCOIL, VOLTUM, ANGLUM, NODFUM, NODMUM,                           M31.3156
     7 KUMOUT, JUMOUT, UMOUTP)                                          M31.3157
      IMPLICIT REAL*8 (A-H, O-Z) ,
     1      INTEGER*4 (I-N)
      DIMENSION  REACL(1), GPAR(1), FPAR(1), HIST(1), UMCURP(1)         M33.1448
      DIMENSION  NODVO1(1), NODVO2(1), JCLTAC(1), JCLOUT(1)             M27.1358
      DIMENSION  JTYPE(1), NODOM(1), JTMTAC(1), HISTOM(1)               M27.1359
      DIMENSION  OMEGM(1), OMOLD(1), THETAM(1)                          M27.1360
      DIMENSION  REAMDU(1), REAMDS(1), FLXDS(1), FLXDR(1)               M27.1361
      DIMENSION  REAMQU(1), FLXQS(1), FLXQR(1)                          M27.1362
      DIMENSION  JCDSAT(1), JCQSAT(1), FLXD(1), FLXQ(1)                 M27.1363
      DIMENSION  NPPAIR(1), ROTMOM(1), NCLD(1)                          M27.1364
      DIMENSION  NCLQ(1), JTQOUT(1), JOMOUT(1), JTHOUT(1)               M27.1365
      DIMENSION  REAMQS(1), EPSOM(1), DCOEF(1), KCOIL(1)                M27.1366
      DIMENSION  VOLTUM(1), ANGLUM(1), NODFUM(1), NODMUM(1)             M31.3158
      DIMENSION  KUMOUT(1), JUMOUT(1), UMOUTP(1)                        M27.1367
      INCLUDE  'BLKCOM.FOR;/NOLIST'                                           
      INCLUDE  'LABCOM.FOR;/NOLIST'                                           
      INCLUDE  'UMDECK.FOR;/NOLIST'                                           
      DIMENSION  NSUBKM(1)                                              M37.5032
      EQUIVALENCE ( KKNONL(1), NSUBKM(1) )                              M37.5033
C  JCLTAC(KCL AND KCL+1) ARE DEFINED AND INITIALIZED IN UMRENU          M39.2713
C  JCLTAC(KCL+2) IS DEFINED HERE. IT IS ZERO UNLESS A SET OF            M39.2714
C     OF MAX 3 UM'S SHARING A COMMON MECH NETW IS DEALT WITH.           M39.2715
C   * IT IS SET TO THE UM NR IF THE UM WITH THIS KCL IS THE             M39.2716
C     LOWEST NUMBERED UM IN THIS SET. THIS UM TAKES CARE OF             M39.2717
C     THE MULTI- PHASE COMPENSATION OF THE COMMON MECH NETW.            M39.2718
C   * IT IS SET TO THE NEGATIVE VALUE OF THE "CURSUB" ENTRY IF          M39.2719
C     IT IS NOT THE LOWEST NUMBERED UM IN THIS SET.                     M39.2720
C  NSHARE IS A FLAG TO DECIDE HOW MECH NETWORKS OF EACH UM ARE          M39.2721
C     TO BE COMPENSATED.                                                M39.2722
C     NSHARE = 1  : NO COMP OF MECH NETWORK IN CASE THAT IT             M39.2723
C                 HAS BEEN DONE EARLIER BY A MECHANICALLY               M39.2724
C                 CONNECTED LOWER-NUMBERED UM                           M39.2725
C     NSHARE = 0 : UM NOT MECHANICALLY CONNECTED TO OTHER UM'S          M39.2726
C                 AND HENCE SINGLE-PHASE COMP AS USUAL.                 M39.2727
C     NSHARE = 10 : COMPENSATION OF COMMON MECH NETWRK TO BE            M39.2728
C                   CONDUCTED WITH THE LOWEST NUMBERED UM.              M39.2729
      IF ( IPRSUP  .GE.  1 )                                            M32.4941
     1 WRITE ( LUNIT6, 4567 )                                           M32.4942
 4567 FORMAT ( 24H  "BEGIN MODULE UMINIT." )                            M32.4943
      ISTART = 0                                                        M27.1371
      KSUBUM = NUMSUB + 1                                               M27.1372
      NCLCOM = N15 + 5                                                          
      IF ( IPRSUP  .GE.  1 )                                            M27.1373
     1 WRITE (LUNIT6, 7302)  KSUBUM                                     M27.1374
 7302 FORMAT ( /,  27H BEGIN  "UMINIT" . KSUBUM =,  I4  )               M27.1375
C  NCOMP IS THE MAX NUMBER OF COMPENSATED PHASES PER SUBNETWORK.        M33.1449
C  N8 IS USED TO DETERMINE THE VALUE OF THIS NCOMP.                     M33.1450
 7000 DO 7010 K = 1,NUMUM                                               M39.2731
      KCL = KCOIL(K)                                                    M39.2732
      JCLTAC(KCL+2) = 0                                                 M39.2733
      N2 = JCLTAC(KCL)                                                          
      N3 = JCLTAC(KCL+1)                                                        
      N4 = K                                                                    
      IF (N2 .EQ. 0 .AND. N3 .EQ. 0) GO TO 7010                         M39.2737
      IF (N2 .NE. 0 .AND. N2 .LT. N4) N4 = N2                           M39.2738
      IF (N3 .NE. 0 .AND. N3 .LT. N4) N4 = N3                           M39.2739
      IF (N4 .EQ. K) JCLTAC(KCL+2) = K                                  M39.2740
 7010 CONTINUE                                                          M39.2741
 7020 DO 7330  K=1, NUMUM                                                       
      KCL = KCOIL(K)                                                    M39.2743
      NSHARE = JCLTAC(KCL) + JCLTAC(KCL+1)                              M39.2744
      IF (NSHARE .NE. 0) NSHARE = 1                                     M39.2745
      IF (JCLTAC(KCL+2) .GT. 0) NSHARE = 10                             M39.2746
C  COMPENSATION OF POWER NETWORK *******************************        M39.2747
      N8 = 0                                                            M39.2748
      NUMSUB = NUMSUB + 1                                               M27.1377
      ISUBEG(NUMSUB) = N15 + 5                                          M37.5034
      N5 = KCL                                                          M39.2749
      N6 = N5 + 2                                                       M27.1380
      N16 = 0                                                           M30. 508
      NUMCOM = K                                                                
      IF (LOOPSS(8) .EQ. 1) GO TO 7310                                  M33.1451
      J = N5                                                            M37.5035
      MARS = 1                                                          M37.5036
 1111 IF ( NODVO1(J)  .EQ.  NODVO2(J) )   GO TO 7305                    M37.5037
 6666 N8  =  N8 + 1                                                     M37.5038
      N15 = N15 + 5                                                     M37.5039
      NSUBKM(N15) = N15 + 5                                             M37.5040
 6767 NSUBKM(N15+1) = NODVO1(J)                                         M37.5041
      NSUBKM(N15+2) = NODVO2(J)                                         M37.5042
C CODE SECTION REPEATEDLY CALLED FOR EACH COMPENSATED NODE-PAIR:        M39.2751
 6868 NSUBKM(N15+3) = - K                                               M39.2752
      NSUBKM(N15+4) = MARS                                              M37.5044
      N16 = 1                                                           M37.5045
      NCLCOM = NCLCOM + 1                                               M39.2753
      NUM = 0                                                           M37.5046
      MARK = KPSOUR(NSUBKM(N15+1))                                      M37.5047
 1010 IF ( MARK .EQ. 0  .OR.  MARK .EQ. NUMSUB ) GO TO 1515             M37.5048
      IF ( MARK .LT. NTOT ) GO TO 7777                                  M37.5049
      DO 1314 MM = 2, KPARTB                                            M37.5050
 1314 IF ( KPSOUR(MM) .EQ. MARK ) KPSOUR(MM) = NUMSUB                   M37.5051
 1515 IF ( NUM .GT. 0 ) GO TO 1616                                      M37.5052
      NUM = 1                                                           M37.5053
      MARK = KPSOUR(NSUBKM(N15+2))                                      M37.5054
      GO TO 1010                                                        M37.5055
 7777 WRITE(LUNIT6,8888) K                                              M39.2754
 8888 FORMAT(/,42H ERROR STOP. SOMETHING HAS GONE WRONG WITH,           M39.2755
     1         45H SETTING UP THE COMPENSATION TABLES IN MODULE,        M39.2756
     2         18H "UMINIT" FOR UM -,I4,14H. CONSULT EMTP               M39.2757
     3   ,/,   42H MANAGEMENT AFTER MAKING SURE THAT YOU DID,           M39.2758
     4         38H NOT VIOLATE THE UM RULE REGARDING THE,               M39.2759
     5         25H THREE-PHASE RESTRICTION.)                            M39.2760
      KILL = 229                                                        M39.2761
      N15 = ISUBEG(MARK)                                                M37.5057
      LSTAT(15) = K                                                     M37.5058
      LSTAT(16) = NSUBKM(N15+3)                                         M37.5059
      GO TO 9800                                                        M37.5060
 1616 GO TO (7305,7312,7318,7322), MARS                                 M39.2762
C  END CODE SECTION ............................................        M39.2763
 7305 J = J + 1                                                         M37.5062
      IF ( J .LE. N6 ) GO TO 1111                                       M37.5063
C  THE FOLLOWING CONDITION IS NEEDED FOR THE LAST NODE-PAIR             M39.2764
C    TO BE COMPENSATED FOR THIS SUBNETWORK. THIS CONDITION IS           M39.2765
C    TO BE INSERTED FOR ALL THE OTHER SUBNETWORKS.                      M39.2766
      IF (N16 .EQ. 1) NSUBKM(N15) = ISUBEG(NUMSUB)                      M39.2767
      IF ( N8  .GT.  NCOMP )   NCOMP = N8                               M27.1389
C  COMPENSATION OF EXCIT NETWORK FOR 3 OR LESS COILS ***********        M39.2768
      N8 = 0                                                            M33.1452
      IF (N16 .EQ. 0) GO TO 7310                                        M33.1453
      NUMSUB = NUMSUB + 1                                               M27.1390
      ISUBEG(NUMSUB) = N15 + 5                                          M37.5065
 7310 N16 = 0                                                           M31.3164
      N6 = N5 + 2 + NCLD(K) + NCLQ(K)                                   M27.1392
      IF (JTYPE(K) .EQ. 4) N6 = N6 + 1                                  M27.1393
      N7 = N6 - N5 + 1                                                  M27.1394
      N9 = N6                                                           M27.1395
      IF (N7 .GT. 6) N9 = N5 + 5                                        M27.1396
      N5 = N5 + 3                                                       M27.1397
      J = N5                                                            M37.5066
      MARS = 2                                                          M37.5067
 2222 IF ( J .GT. N9 ) GO TO 2323                                       M37.5068
      IF (NODVO1(J) .NE. NODVO2(J)) GO TO 6666                          M37.5069
 7312 J = J + 1                                                         M37.5070
      GO TO 2222                                                        M37.5071
 2323 IF ( N16 .EQ. 1 ) NSUBKM(N15) = ISUBEG(NUMSUB)                    M37.5072
      IF (N8 .GT. NCOMP) NCOMP = N8                                     M33.1455
C  COMPENSATION OF EXCIT NETWORK FOR 4 OR MORE COILS ***********        M39.2769
C    NOTE : IN THIS CASE THEY WILL ALL BE SINGLE-PHASE COMP             M39.2770
      N8 = 0                                                            M33.1456
      IF (N7 .LE. 6) GO TO 7319                                         M31.3168
      N5 = N5 + 3                                                       M27.1405
      J = N5                                                            M37.5073
      MARS = 3                                                          M37.5074
 3333 IF ( J .GT. N6 ) GO TO 3434                                       M37.5075
      IF (NODVO1(J) .EQ. NODVO2(J)) GO TO 7318                          M31.3170
      IF (N16 .EQ. 0) GO TO 7316                                        M33.1457
 7315 NUMSUB = NUMSUB + 1                                               M31.3173
      ISUBEG(NUMSUB) = N15 + 5                                          M37.5076
 7316 N15 = N15 + 5                                                     M37.5077
      N8 = 1                                                            M33.1458
      NSUBKM(N15) = N15                                                         
      GO TO 6767                                                        M37.5079
 7318 J = J + 1                                                         M37.5080
      GO TO 3333                                                        M37.5081
 3434 IF (N8 .GT. NCOMP) NCOMP = N8                                     M37.5082
C  COMPENSATION OF MECH NETWORK ********************************        M39.2772
 7319 N8 = 0                                                            M39.2773
      IF (JTMTAC(K) .NE. 0) GO TO 7328                                  M39.2774
      IF (NSHARE .EQ. 1) GO TO 7328                                             
      MARS = 4                                                          M39.2776
      IF (N16 .EQ. 0) GO TO 7320                                        M33.1461
      NUMSUB = NUMSUB + 1                                               M27.1415
      ISUBEG(NUMSUB) = N15 + 5                                          M37.5083
 7320 N15 = N15 + 5                                                     M37.5084
      N8 = N8 + 1                                                       M39.2777
      NSUBKM(N15) = N15 + 5                                             M39.2778
      NSUBKM(N15+1) = NODOM(NUMCOM)                                     M39.2779
      NSUBKM(N15+2) = 1                                                 M37.5087
      GO TO 6868                                                        M37.5089
C  COMPENSATION OF HIGHER NUMBERED UM IF MECHANICALLY CONNECTED         M39.2780
 7322 IF (NSHARE .EQ. 0) GO TO 7325                                     M39.2781
      IF (NSHARE .EQ. -1) GO TO 7324                                            
      NSHARE = 0                                                                
      IF (JCLTAC(KCL+1) .NE. 0) NSHARE = - 1                                    
      NUMCOM = JCLTAC(KCL)                                              M39.2785
      IF (NUMCOM .EQ. 0) GO TO 7324                                     M39.2786
      N1 = KCOIL(NUMCOM)                                                M39.2787
      JCLTAC(N1+2) = - NCLCOM                                           M39.2788
      GO TO 7320                                                        M39.2789
 7324 NUMCOM = JCLTAC(KCL+1)                                            M39.2790
      NSHARE = 0                                                                
      N1 = KCOIL(NUMCOM)                                                M39.2792
      JCLTAC(N1+2) = - NCLCOM                                           M39.2793
      GO TO 7320                                                        M39.2794
 7325 IF (N16 .EQ. 1) NSUBKM(N15) = ISUBEG(NUMSUB)                      M39.2795
      IF (N8 .GT. NCOMP) NCOMP = N8                                     M39.2796
 7328 IF (N16 .EQ. 0)  NUMSUB = NUMSUB - 1                              M39.2797
 7330 CONTINUE                                                          M31.3183
 9800 IF ( IPRSUP  .GE.  1 )                                            M37.5091
     1 WRITE ( LUNIT6, 4568 )                                           M32.4945
 4568 FORMAT ( 24H  "EXIT  MODULE UMINIT." )                            M32.4946
      RETURN                                                            M27.1421
      END                                                               M27.1422
