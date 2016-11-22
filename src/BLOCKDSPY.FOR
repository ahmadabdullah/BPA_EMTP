      BLOCK DATA BLKHLP
      INCLUDE  'DEKSPY.FOR;'
      DATA  TEXSPY (   1 )   /  'KEY WORD NO.  1:  "HEADING"     ----  -
     1---  ----                                '  /
      DATA  TEXSPY (   2 )   /  '  Response will be the printing of the 
     1previously-defined heading of  "EXAMINE",'  /
      DATA  TEXSPY (   3 )   /  '  followed by current values of all var
     1iables (as for the 1st  "EXAMINE"  use). '  /
      DATA  TEXSPY (   4 )   /  'KEY WORD NO.  2:  "STOP"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY (   5 )   /  '  This command will terminate interacti
     1ve EMTP execution immediately,  by  means'  /
      DATA  TEXSPY (   6 )   /  '  of a FORTRAN "STOP" statement.  There
     1 will be no automatic saving  of  tables,'  /
      DATA  TEXSPY (   7 )   /  '  or  of plot data points before such a
     1 termination  (use prior  "SLEEP"  and/or'  /
      DATA  TEXSPY (   8 )   /  '  "LUNIT4"  commands, if such preservat
     1ion is desired).                         '  /
      DATA  TEXSPY (   9 )   /  'KEY WORD NO.  3:  "PLOT"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY (  10 )   /  '  Issue this command to transfer contro
     1l to the  "OUTER:"  prompt of interactive'  /
      DATA  TEXSPY (  11 )   /  '  EMTP plotting (the former separate EM
     1TP CRT plotting program  "TPPLOT").   Due'  /
      DATA  TEXSPY (  12 )   /  '  to the absorbtion into SPY,  several 
     1changes have been made.  First,  no plot-'  /
      DATA  TEXSPY (  13 )   /  '  file specification is required of the
     1 user  (he should subsequently send  "GO"'  /
      DATA  TEXSPY (  14 )   /  '  if no other OUTER-level response is d
     1esired).   At the  "MIDDLE:"  level,  the'  /
      DATA  TEXSPY (  15 )   /  '  "TIMESPAN"  computation is now automa
     1tically performed,  internally.   At  any'  /
      DATA  TEXSPY (  16 )   /  '  level,  "STOP"  no longer terminates 
     1program execution, but instead it returns'  /
      DATA  TEXSPY (  17 )   /  '  control to the  "SPY:"  prompt.   Con
     1tinuous,  automatic plotting  of  the on-'  /
      DATA  TEXSPY (  18 )   /  '  going solution  (like a strip-chart) 
     1 is  based  on the use of either  "ROLLC"'  /
      DATA  TEXSPY (  19 )   /  '  (for character plotting)  or  "ROLLV"
     1  (for vector plotting)  as  commands  at'  /
      DATA  TEXSPY (  20 )   /  '  the  "INNER:"  level.   For  a  detai
     1led explanation of all  "PLOT"  commands,'  /
      DATA  TEXSPY (  21 )   /  '  send  "HELP"  at any of the three lev
     1els within  "PLOT".                      '  /
      DATA  TEXSPY (  22 )   /  'KEY WORD NO.  4:  "HELP"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY (  23 )   /  '  Education for the ignorant, such as t
     1he user is now being subjected to (joke).'  /
      DATA  TEXSPY (  24 )   /  '  A  carriage return  <CR>  will  produ
     1ce text for the next key word  in  order,'  /
      DATA  TEXSPY (  25 )   /  '  while  a  return  to the  "SPY:"  pro
     1mpt is accomplished by  "SPY", or  "END".'  /
      DATA  TEXSPY (  26 )   /  '  "ALL"  will loop over  all  explanati
     1ons  (subject  to  user-keyed interrupt).'  /
      DATA  TEXSPY (  27 )   /  '  Sending  "TOP"  will rewind to the fi
     1rst message, while  "BOT"  will give the '  /
      DATA  TEXSPY (  28 )   /  '  last.   Use  "BACK"  to back up one m
     1essage.                                  '  /
      DATA  TEXSPY (  29 )   /  'KEY WORD NO.  5:  "EXAMINE"     ----  -
     1---  ----                                '  /
      DATA  TEXSPY (  30 )   /  '  Issue this command  to  examine the c
     1ontents of any EMTP COMMON variables  (of'  /
      DATA  TEXSPY (  31 )   /  '  solution overlays).   INTEGER  scalar
     1s require 6 columns;  all other variables'  /
      DATA  TEXSPY (  32 )   /  '  require 15.   Subsequent prompts will
     1 allow  the  user  to specify scalars and'  /
      DATA  TEXSPY (  33 )   /  '  vector ranges (e.g.,  "KBUS(3:8)"  fo
     1r  cells  3  through  8 of KBUS).   "END"'  /
      DATA  TEXSPY (  34 )   /  '  terminates the list,  resulting  in a
     1 display of heading and numerical values.'  /
      DATA  TEXSPY (  35 )   /  '  Any later striking of the  "RETURN"  
     1key  will  then display current numerical'  /
      DATA  TEXSPY (  36 )   /  '  values only.   Send  "HEADING"  for  
     1a  refresh of the variable names.  For  a'  /
      DATA  TEXSPY (  37 )   /  '  rolling display,  send  "ROLL"  (also
     1,  see  separate  instructions  for  this'  /
      DATA  TEXSPY (  38 )   /  '  command).  For  ROLLing, the  output 
     1vector  is  re-formed at each SPY chance,'  /
      DATA  TEXSPY (  39 )   /  '  but is only output when one or more v
     1ariables has changed.  To  terminate  the'  /
      DATA  TEXSPY (  40 )   /  '  ROLL-loop,  use the regular user-keye
     1d interrupt.                             '  /
      DATA  TEXSPY (  41 )   /  'KEY WORD NO.  6:  "DEPOSIT"     ----  -
     1---  ----                                '  /
      DATA  TEXSPY (  42 )   /  '  Issue this command to modify the cont
     1ents of any  EMTP  COMMON  variables  (of'  /
      DATA  TEXSPY (  43 )   /  '  solution overlays).   Subsequent prom
     1pts  will  permit  the  user  to  specify'  /
      DATA  TEXSPY (  44 )   /  '  scalars  and vector ranges  (e.g.,  "
     1KBUS(3:8)"  for  cells  3  through  8  of'  /
      DATA  TEXSPY (  45 )   /  '  KBUS).   "END"  terminates the list, 
     1 returning to the  "SPY:"  prompt.  After'  /
      DATA  TEXSPY (  46 )   /  '  each  variable,  there will be a prom
     1pt for the desired new value (free-format'  /
      DATA  TEXSPY (  47 )   /  '  number),  if no  "="  is used.   The 
     1separately-prompted input  is  rigorously'  /
      DATA  TEXSPY (  48 )   /  '  free-format  (so  that any  I, F, or 
     1E-field number is permissible).  But  for'  /
      DATA  TEXSPY (  49 )   /  '  simple numeric values which  can  be 
     1 read  using  F15.0,  or  for  text (A6),'  /
      DATA  TEXSPY (  50 )   /  '  follow the scalar or vector by an equ
     1al sign and then the number.  The case of'  /
      DATA  TEXSPY (  51 )   /  '  ALPHANUMERIC  must  not  have imbedde
     1d blanks after the  "=",  but numbers can'  /
      DATA  TEXSPY (  52 )   /  '  (within the span of 15 columns).     
     1                                         '  /
      DATA  TEXSPY (  53 )   /  'KEY WORD NO.  7:  "SWITCH"      ----  -
     1---  ----                                '  /
      DATA  TEXSPY (  54 )   /  '  This response to the  "SPY:"  prompt 
     1 is  issued  for  a  display  of the EMTP'  /
      DATA  TEXSPY (  55 )   /  '  switch  table.   Subsequently  send  
     1an  additional  "EXTRA"  to change to the'  /
      DATA  TEXSPY (  56 )   /  '  next sub-table (as of February 1984, 
     1there are two),  if different columns are'  /
      DATA  TEXSPY (  57 )   /  '  desired.  Within any one choice as to
     1 sub-table,  there is a loop in which the'  /
      DATA  TEXSPY (  58 )   /  '  user sends a pair of free-format begi
     1nning and ending row numbers for display,'  /
      DATA  TEXSPY (  59 )   /  '  or  a  key word.  Recognized key word
     1s here  are  "ALL"  for the entire table,'  /
      DATA  TEXSPY (  60 )   /  '  "TOP"  for row one,  "BOT"  for the  
     1last  row,  "NEXT"  or  just  a  carriage'  /
      DATA  TEXSPY (  61 )   /  '  return for the following row, and of 
     1course  "SPY"  to  abort the display loop'  /
      DATA  TEXSPY (  62 )   /  '  and return to the  "SPY:"  prompt.  T
     1o refresh the heading, use either  "SWIT"'  /
      DATA  TEXSPY (  63 )   /  '  or  "HEAD"  (only 4 characters of "SW
     1ITCH" or "HEADING" are checked).         '  /
      DATA  TEXSPY (  64 )   /  'KEY WORD NO.  8:  "APPEND"      ----  -
     1---  ----                                '  /
      DATA  TEXSPY (  65 )   /  '  This command is the gateway to instal
     1lation-dependent commands which are non- '  /
      DATA  TEXSPY (  66 )   /  '  standard,  and which have been instal
     1led locally.   The  UTPF carries a dummy '  /
      DATA  TEXSPY (  67 )   /  '  SUBROUTINE APPEND,  by  definition.  
     1 One  clean  way  of adding a functional '  /
      DATA  TEXSPY (  68 )   /  '  module is at translation time,  using
     1 a  SUBMOD  request.                     '  /
      DATA  TEXSPY (  69 )   /  'KEY WORD NO.  9:  "SAVE"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY (  70 )   /  '  When the EMTP is in the time-step loo
     1p, this command will force an exit to    '  /
      DATA  TEXSPY (  71 )   /  '  overlay 20 upon completion of present
     1 time-step.   There,  "KATALG"  saves    '  /
      DATA  TEXSPY (  72 )   /  '  EMTP tables as rapidly as possible (m
     1ay not be permanent).   The simulation   '  /
      DATA  TEXSPY (  73 )   /  '  then recommences by an automatic tran
     1sfer back to the beginning of the        '  /
      DATA  TEXSPY (  74 )   /  '  time-step loop ("OVER16").   Since pl
     1ot data points are not part of the       '  /
      DATA  TEXSPY (  75 )   /  '  saving, if the user wants these to be
     1 preserved, too, he must be careful.     '  /
      DATA  TEXSPY (  76 )   /  '  Provided EMTP storage for plot data p
     1oints does not fill up before the        '  /
      DATA  TEXSPY (  77 )   /  '  subsequent  RESTORE  operation (to ba
     1ck up time to the  SAVE  point), there   '  /
      DATA  TEXSPY (  78 )   /  '  is no problem.  Otherwise, consider u
     1se of  "SPACE"  and  "LUNIT4"  commands. '  /
      DATA  TEXSPY (  79 )   /  'KEY WORD NO. 10:  "RESTORE"     ----  -
     1---  ----                                '  /
      DATA  TEXSPY (  80 )   /  '  The  "RESTORE"  command is the revers
     1e of  "SAVE".  When within the DELTAT    '  /
      DATA  TEXSPY (  81 )   /  '  loop, use of  "RESTORE"  will rapidly
     1 transfer to overlay 20.  There, module  '  /
      DATA  TEXSPY (  82 )   /  '  "KATALG" restores former tables, afte
     1r which control is transferred to SPY.   '  /
      DATA  TEXSPY (  83 )   /  '  Possibly the  "SPACE"  and  "LUNIT4" 
     1 would also be appropriate, to restore   '  /
      DATA  TEXSPY (  84 )   /  '  any separately plot files.   Also aft
     1er tables have been restored, the user   '  /
      DATA  TEXSPY (  85 )   /  '  can modify any EMTP variables  desire
     1d,  and exit  "DEPOSIT"  with  "END";    '  /
      DATA  TEXSPY (  86 )   /  '  finally, send  "GO"  in response to t
     1he  "SPY:"  prompt, to transfer back to  '  /
      DATA  TEXSPY (  87 )   /  '  the top of the time-step loop ("OVER1
     16").                                     '  /
      DATA  TEXSPY (  88 )   /  'KEY WORD NO. 11:  "GO"          ----  -
     1---  ----                                '  /
      DATA  TEXSPY (  89 )   /  '  This command is issued in response to
     1 the  "SPY:"  prompt to terminate        '  /
      DATA  TEXSPY (  90 )   /  '  several sequences, such as one which 
     1might have begun with  "RESTORE".  In    '  /
      DATA  TEXSPY (  91 )   /  '  this case, there would be the precedi
     1ng transfers of control from overlay 16  '  /
      DATA  TEXSPY (  92 )   /  '  to overlay 20 ("KATALG"), and then ba
     1ck to overlay 16 again.  As a second     '  /
      DATA  TEXSPY (  93 )   /  '  example,  "GO"  cancels the  "REST"  
     1command.  Third, it can be used to       '  /
      DATA  TEXSPY (  94 )   /  '  begin the EMTP solution following the
     1  "DATA"  command (for data card input   '  /
      DATA  TEXSPY (  95 )   /  '  at the beginning of program execution
     1).   Finally,  "GO"  cancels the         '  /
      DATA  TEXSPY (  96 )   /  '  suspension   of execution which accom
     1panies the  "SLEEP"  command.            '  /
      DATA  TEXSPY (  97 )   /  'KEY WORD NO. 12:  "ECHO"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY (  98 )   /  '  Users who are interested in keeping a
     1 history of SPY commands which are to    '  /
      DATA  TEXSPY (  99 )   /  '  be issued can use the  "ECHO"  comman
     1d.   A subsequent prompt then allows     '  /
      DATA  TEXSPY ( 100 )   /  '  several choices.  To begin such accum
     1ulation, send  "BEGIN";  to end it,      '  /
      DATA  TEXSPY ( 101 )   /  '  send  "FILE",  which will result in t
     1he dumping of all accumulation onto      '  /
      DATA  TEXSPY ( 102 )   /  '  disk as a permanent file of the users
     1 choice (a subsequent prompt will        '  /
      DATA  TEXSPY ( 103 )   /  '  ask for the desired file name).   The
     1 history consists of 80-column card      '  /
      DATA  TEXSPY ( 104 )   /  '  images,  stored from the bottom of  F
     1ILE6  upward  (with cell  KSPSAV         '  /
      DATA  TEXSPY ( 105 )   /  '  storing the last, and  LIMCRD  storin
     1g the first).  To view the accumulation  '  /
      DATA  TEXSPY ( 106 )   /  '  to date, use  "SHOW".   When  "FILE" 
     1 is used,  not only is a copy sent to    '  /
      DATA  TEXSPY ( 107 )   /  '  disk, but those in memory are erased.
     1   Hence, if  ECHOing  is to continue,   '  /
      DATA  TEXSPY ( 108 )   /  '  the user must send  "BEGIN"  again,  
     1immediately after  "FILE"  is complete.  '  /
      DATA  TEXSPY ( 109 )   /  '  In order to erase an erroneous, immed
     1iately-preceding command, use  "CANCEL"  '  /
      DATA  TEXSPY ( 110 )   /  '  at any point.  This is intercepted by
     1 the input routine  "FLAGER",  so is     '  /
      DATA  TEXSPY ( 111 )   /  '  not a SPY command per se.   There wil
     1l be confirmation of the erasure.        '  /
      DATA  TEXSPY ( 112 )   /  'KEY WORD NO. 13:  "FIND"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 113 )   /  '  The sending of  "FIND"  will result i
     1n the printing of a heading, followed    '  /
      DATA  TEXSPY ( 114 )   /  '  by a pause, as  SPY  waits for the us
     1er to supply a 6-character EMTP symbol   '  /
      DATA  TEXSPY ( 115 )   /  '  name.   This is a closed loop: after 
     1receiving a name,  SPY  will display     '  /
      DATA  TEXSPY ( 116 )   /  '  the memory address, and then wait for
     1 another such symbol.   Exit by  "END".  '  /
      DATA  TEXSPY ( 117 )   /  '  Wild cards ("*") of  VAX/VMS  are hon
     1ored here, although the candidate        '  /
      DATA  TEXSPY ( 118 )   /  '  string is limited to eight characters
     1 maximum (only the first 8 are read).    '  /
      DATA  TEXSPY ( 119 )   /  'KEY WORD NO. 14:  "LIST"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 120 )   /  '  This command will result in the print
     1ing of a heading, followed by a pause.   '  /
      DATA  TEXSPY ( 121 )   /  '  At this point the user is in a loop, 
     1supplying row numbers (beginning and     '  /
      DATA  TEXSPY ( 122 )   /  '  ending rows as a pair of free-format 
     1integers).   SPY responds to each with   '  /
      DATA  TEXSPY ( 123 )   /  '  a display of those rows of the SPY sy
     1mbol table, and then waits for the next  '  /
      DATA  TEXSPY ( 124 )   /  '  request.   The sending of  "0,0"  wil
     1l break out, returning to the  "SPY:"    '  /
      DATA  TEXSPY ( 125 )   /  '  prompt.   The user can interrupt any 
     1excessively long display with his keyed  '  /
      DATA  TEXSPY ( 126 )   /  '  interrupt (details depend upon comput
     1er).  Instead of  "0,0"  to break out,   '  /
      DATA  TEXSPY ( 127 )   /  '  "END"  or  "SPY:"  can alternatively 
     1be used.  Sending nothing (just a        '  /
      DATA  TEXSPY ( 128 )   /  '  carriage return <CR>) is interpreted 
     1by SPY as a request for "more of the     '  /
      DATA  TEXSPY ( 129 )   /  '  same" ---- the same number of rows as
     1 just displayed, beginning where the     '  /
      DATA  TEXSPY ( 130 )   /  '  last display left off.   Special tric
     1kery is required if an argument of  "@"  '  /
      DATA  TEXSPY ( 131 )   /  '  usage is to respond to the  "LIST"  p
     1rompt,  since a comma must not be used   '  /
      DATA  TEXSPY ( 132 )   /  '  as the free-format separator (due to 
     1confusion with use of the same symbol    '  /
      DATA  TEXSPY ( 133 )   /  '  for argument separation by "@").  Ins
     1tead, a pounds sign  "#"  must be used   '  /
      DATA  TEXSPY ( 134 )   /  '  rather than a blank (due to extractio
     1n of blanks by  "@"  logic).  Finally,   '  /
      DATA  TEXSPY ( 135 )   /  '  sending  "ALL"  instead of two row nu
     1mbers displays the whole table.          '  /
      DATA  TEXSPY ( 136 )   /  'KEY WORD NO. 15:  "SPY"         ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 137 )   /  '  This text, supplied almost anywhere t
     1hat the program is looking for text,     '  /
      DATA  TEXSPY ( 138 )   /  '  will cause an internal interruption o
     1f whatever was happening, and a return   '  /
      DATA  TEXSPY ( 139 )   /  '  to the  "SPY:"  prompt.   One excepti
     1on is the  "HELP"  prompt which is now   '  /
      DATA  TEXSPY ( 140 )   /  '  being serviced.                      
     1                                         '  /
      DATA  TEXSPY ( 141 )   /  'KEY WORD NO. 16:  "BREAK"       ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 142 )   /  '  This response to the  "SPY:"  prompt 
     1is appropriate if the user wants the     '  /
      DATA  TEXSPY ( 143 )   /  '  simulation to continue uninterrupted 
     1until some pre-specified time, when a    '  /
      DATA  TEXSPY ( 144 )   /  '  clean break at the start of "SUBTS1" 
     1will be made.  A subsequent prompt will  '  /
      DATA  TEXSPY ( 145 )   /  '  allow the user to specify the future 
     1break time  T-break  in seconds.  If a   '  /
      DATA  TEXSPY ( 146 )   /  '  minus sign is appended, then the inpu
     1t is taken to be a step number of the    '  /
      DATA  TEXSPY ( 147 )   /  '  time-step loop, and the program will 
     1calculate  T-break   by multiplying by   '  /
      DATA  TEXSPY ( 148 )   /  '  DELTAT.  Oh, in case "SUBTS1" means n
     1othing to the user, this is the first of '  /
      DATA  TEXSPY ( 149 )   /  '  four pieces of overlay 16 (the time-s
     1tep loop).  In case the user sends zero  '  /
      DATA  TEXSPY ( 150 )   /  '  for  T-break, an added prompt will se
     1ek clarification as to which UTPF overlay'  /
      DATA  TEXSPY ( 151 )   /  '  is the desired stopping point (immedi
     1ately prior to entry).  Common usage     '  /
      DATA  TEXSPY ( 152 )   /  '  involves overlays numbered 6  (after 
     1all sources have been read),  12  (when  '  /
      DATA  TEXSPY ( 153 )   /  '  the   phasor solution is complete), o
     1r  16  (the time-step loop).             '  /
      DATA  TEXSPY ( 154 )   /  'KEY WORD NO. 17:  "WHEN"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 155 )   /  '  This response to the  "SPY:"  prompt 
     1will redefine the EMTP table-handling    '  /
      DATA  TEXSPY ( 156 )   /  '  time of  "SAVE"  and  "RESTORE" .   T
     1hat is,  if so defined,  this overrides  '  /
      DATA  TEXSPY ( 157 )   /  '  the otherwise immediate exit of the t
     1ime-step loop for table-handling.        '  /
      DATA  TEXSPY ( 158 )   /  'KEY WORD NO. 18:  "COMMENT"     ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 159 )   /  '  This response to the  "SPY:"  prompt 
     1will toggle the switch which controls the'  /
      DATA  TEXSPY ( 160 )   /  '  printing of comment cards ("C ") whic
     1h may be contained within command files. '  /
      DATA  TEXSPY ( 161 )   /  '  The default (starting value) is to sh
     1ow comment cards during execution ("@"). '  /
      DATA  TEXSPY ( 162 )   /  'KEY WORD NO. 19:  "@?"          ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 163 )   /  '  This response to the  "SPY:"  prompt 
     1will result in the internal  OPENing  of '  /
      DATA  TEXSPY ( 164 )   /  '  an arbitary disk file, and the connec
     1tion of this file as replacement for     '  /
      DATA  TEXSPY ( 165 )   /  '  keyboard input to SPY.  If the file n
     1ame consists of just a single digit      '  /
      DATA  TEXSPY ( 166 )   /  '  (?=1-9),  INCLSPY?.DAT  is the disk f
     1ile name to be used.  READing from the   '  /
      DATA  TEXSPY ( 167 )   /  '  disk file continues until an end-of-f
     1ile is hit, at which point the SPY input '  /
      DATA  TEXSPY ( 168 )   /  '  channel is connected once again to th
     1e keyboard.  Such usage can not be nested'  /
      DATA  TEXSPY ( 169 )   /  '  (i.e., no such disk file can itself c
     1ontain an  "@"  statement).   EMTP       '  /
      DATA  TEXSPY ( 170 )   /  '  comment cards ("C ") are permitted wi
     1thin such disk files, however,  with the '  /
      DATA  TEXSPY ( 171 )   /  '  "COMMENT"  switch controlling whether
     1 or not they are written to the screen   '  /
      DATA  TEXSPY ( 172 )   /  '  (the default is for such writing).   
     1Other lines of an executed disk file are '  /
      DATA  TEXSPY ( 173 )   /  '  generally not seen by the user as the
     1y are read during execution, and neither '  /
      DATA  TEXSPY ( 174 )   /  '  are the repetitive  "SPY:"  prompts. 
     1 Parameters are possible, to substitute  '  /
      DATA  TEXSPY ( 175 )   /  '  for 8-column   "%%%%%%%%"   fields of
     1 the disk file.   Blanks are ignored,    '  /
      DATA  TEXSPY ( 176 )   /  '  and arguments are to be separated by 
     1commas.  An opening parenthesis  "("  and'  /
      DATA  TEXSPY ( 177 )   /  '  a closing parenthesis  ")"  are optio
     1nal delimiters.   Each command line is   '  /
      DATA  TEXSPY ( 178 )   /  '  limited to 80 columns (no continuatio
     1n), and a maximum of 10 arguments.   The '  /
      DATA  TEXSPY ( 179 )   /  '  left-to-right list is applied to the 
     1file %-fields from top to bottom, in     '  /
      DATA  TEXSPY ( 180 )   /  '  order.  The %-fields can be built int
     1o any line of the disk file which        '  /
      DATA  TEXSPY ( 181 )   /  '  is read by  SPY  proper (not PLOTting
     1).   Finally, there is the use of a pound'  /
      DATA  TEXSPY ( 182 )   /  '  sign "#" for reserved blanks, which a
     1re otherwise ignored.   The classic case '  /
      DATA  TEXSPY ( 183 )   /  '  where it is needed is for a response 
     1to  "LIST"  (e.g.,  "2#4");   this is    '  /
      DATA  TEXSPY ( 184 )   /  '  free-format information, with a comma
     1 impossible due to the conflict with "@".'  /
      DATA  TEXSPY ( 185 )   /  '  Although not a part of the interactiv
     1e  SPY  code per se, it should also be   '  /
      DATA  TEXSPY ( 186 )   /  '  remembered that commands which could 
     1be built into  "@"  files can also be    '  /
      DATA  TEXSPY ( 187 )   /  '  executed as part of the regular EMTP 
     1data.  The key to such usage is  "$SPY", '  /
      DATA  TEXSPY ( 188 )   /  '  which is processed by  "CIMAGE"  and/
     1or  "EREXIT".   If all SPY commands are  '  /
      DATA  TEXSPY ( 189 )   /  '  to be built in-line as part of the LU
     1NIT5 EMTP input data, then precede such  '  /
      DATA  TEXSPY ( 190 )   /  '  data by a card reading  "$SPY",  with
     1 columns 5 onward blank.  In this        '  /
      DATA  TEXSPY ( 191 )   /  '  case,  "EREXIT"  reads and removes su
     1ch data cards, creating special reserved '  /
      DATA  TEXSPY ( 192 )   /  '  disk files named  SPYFILE?.DAT,  wher
     1e  "?"  is a single digit between one    '  /
      DATA  TEXSPY ( 193 )   /  '  and nine (allowing a maximum of nine 
     1such distinct groups of SPY commands).   '  /
      DATA  TEXSPY ( 194 )   /  '  The last SPY command of each such gro
     1up is to be followed by  "$ENDSPY"  as a '  /
      DATA  TEXSPY ( 195 )   /  '  special marker record.  On the other 
     1hand, if the user does not care about    '  /
      DATA  TEXSPY ( 196 )   /  '  unifying all such EMTP data in a sing
     1le disk file, then a single line of EMTP '  /
      DATA  TEXSPY ( 197 )   /  '  data,  "$SPY, FILENAME",  is all that
     1 is required to provide the connection to'  /
      DATA  TEXSPY ( 198 )   /  '  SPY.  In this second card,  FILENAME 
     1 can be any legal, full file name of the '  /
      DATA  TEXSPY ( 199 )   /  '  computer system being considered (if 
     1the user supplies the file name, and     '  /
      DATA  TEXSPY ( 200 )   /  '  puts commands in the disk file, then 
     1there is no limit on the number of       '  /
      DATA  TEXSPY ( 201 )   /  '  such usages.   The role of  "CIMAGE" 
     1is to treat  "$SPY"  like   "$INCLUDE".  '  /
      DATA  TEXSPY ( 202 )   /  'KEY WORD NO. 20:  "ROLL"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 203 )   /  '  This response to the  "SPY:"  prompt 
     1will result in a "rolling" of previously-'  /
      DATA  TEXSPY ( 204 )   /  '  defined  "EXAMINE"  request.   While 
     1this happens, no other SPY activity is   '  /
      DATA  TEXSPY ( 205 )   /  '  permitted (any <CR> will abort the lo
     1op, and return to the  "SPY:"  prompt).  '  /
      DATA  TEXSPY ( 206 )   /  '  Following  "ROLL",  the simulation wi
     1ll recommence, with SPY called at each   '  /
      DATA  TEXSPY ( 207 )   /  '  opportunity for a SPY break (within f
     1requency  MAXFLG  of  "DEKSPY",  which   '  /
      DATA  TEXSPY ( 208 )   /  '  nominally has the value one, implying
     1 four checks per time step).   The very  '  /
      DATA  TEXSPY ( 209 )   /  '  first check, the heading and current 
     1value of the  "EXAMINE"  vector will be  '  /
      DATA  TEXSPY ( 210 )   /  '  displayed;  thereafter, output will b
     1e produced if and only if a change in    '  /
      DATA  TEXSPY ( 211 )   /  '  the output (compared with the previou
     1s evaluation) has occurred.              '  /
      DATA  TEXSPY ( 212 )   /  '  A final thought is about other comman
     1ds which function much like the  "ROLL"  '  /
      DATA  TEXSPY ( 213 )   /  '  commands, although in fact these are 
     1not issued at the  "SPY:" level.  For    '  /
      DATA  TEXSPY ( 214 )   /  '  "PLOT"  use,  at the  "INNER:"  level
     1,  "ROLLC"  will produce continuous      '  /
      DATA  TEXSPY ( 215 )   /  '  character plotting, while  "ROLLV"  d
     1oes the same for vector plotting.   It   '  /
      DATA  TEXSPY ( 216 )   /  '  is the  "NOROLL"  command of SPY whic
     1h will cancel either or both of these.   '  /
      DATA  TEXSPY ( 217 )   /  'KEY WORD NO. 21:  "TYPE?"       ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 218 )   /  '  This response to the  "SPY:"  prompt 
     1will result in the listing of command    '  /
      DATA  TEXSPY ( 219 )   /  '  file  INCLSPY?.DAT  of  "@?"  usage. 
     1  For any specific numerical digit       '  /
      DATA  TEXSPY ( 220 )   /  '  "?",  just that one file will be list
     1ed.   But if symbolic "?" is retained, or'  /
      DATA  TEXSPY ( 221 )   /  '  if column 5 onward is blank, then all
     1 possible  "@?"  files will be listed    '  /
      DATA  TEXSPY ( 222 )   /  '  in natural order, preceded by an iden
     1tifying heading.  Also, for arbitrary    '  /
      DATA  TEXSPY ( 223 )   /  '  disk files of card images with names 
     1of 2 or more characters, this command    '  /
      DATA  TEXSPY ( 224 )   /  '  will display them.  "TYPE FILENAME"  
     1is the form of this more general         '  /
      DATA  TEXSPY ( 225 )   /  '  command, where  FILENAME  is any lega
     1l file name of 32 or fewer characters.   '  /
      DATA  TEXSPY ( 226 )   /  '  If no such file exists, there will be
     1 a warning message, so this command      '  /
      DATA  TEXSPY ( 227 )   /  '  can be used to check on the existence
     1 of arbitary disk files of card images.  '  /
      DATA  TEXSPY ( 228 )   /  'KEY WORD NO. 22:  "VERIFY"      ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 229 )   /  '  This response to the  "SPY:"  prompt 
     1will toggle the switch that controls the '  /
      DATA  TEXSPY ( 230 )   /  '  echoing of data cards within a disk f
     1ile which is read via  "@"  usage.  The  '  /
      DATA  TEXSPY ( 231 )   /  '  default (beginning) setting is to hav
     1e such echoing.   A related command is   '  /
      DATA  TEXSPY ( 232 )   /  '  "COMMENT",  which can separately cont
     1rol the display of comment ("C ") cards  '  /
      DATA  TEXSPY ( 233 )   /  '  as they are encountered during proces
     1sing of the  "@"  file.  If there is no  '  /
      DATA  TEXSPY ( 234 )   /  '  echoing of SPY data, then comment car
     1ds might likewise not be displayed;  or, '  /
      DATA  TEXSPY ( 235 )   /  '  they can be used as an absolute refer
     1ence (if they are displayed), to mark the'  /
      DATA  TEXSPY ( 236 )   /  '  beginning or ending of invisible oper
     1ations due to no-echoing of "VERIFY".    '  /
      DATA  TEXSPY ( 237 )   /  'KEY WORD NO. 23:  "FILES"       ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 238 )   /  '  This  response  to  the  "SPY:"  prom
     1pt will result in a display of all of the'  /
      DATA  TEXSPY ( 239 )   /  '  INCLSPY?.DAT  files which exist,  bas
     1ed on the FORTRAN "INQUIRE" operation  at'  /
      DATA  TEXSPY ( 240 )   /  '  the time program execution began.  An
     1  "X"  means that the file exists (in the'  /
      DATA  TEXSPY ( 241 )   /  '  display),  whereas a blank means that
     1 it does not.  There are 9 columns.      '  /
      DATA  TEXSPY ( 242 )   /  'KEY WORD NO. 24:  "SLEEP"       ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 243 )   /  '  This response to the  "SPY:"  prompt 
     1will put the EMTP to sleep in such a way '  /
      DATA  TEXSPY ( 244 )   /  '  that the simulation can be continued 
     1at any later time.  The interactive      '  /
      DATA  TEXSPY ( 245 )   /  '  "SLEEP"  command is comparable to the
     1 batch-mode use of miscellaneous data    '  /
      DATA  TEXSPY ( 246 )   /  '  parameter   MEMSAV = 1   for the savi
     1ng of EMTP tables on disk.  To service a '  /
      DATA  TEXSPY ( 247 )   /  '  "SLEEP"  command, SPY exits the time-
     1step loop and jumps to  "OVER20"  for    '  /
      DATA  TEXSPY ( 248 )   /  '  table dumping to disk.  Subsequent aw
     1akening is via  "WAKE",  which reads     '  /
      DATA  TEXSPY ( 249 )   /  '  EMTP tables back from disk into EMTP 
     1memory.  Any plot data must be separately'  /
      DATA  TEXSPY ( 250 )   /  '  and manually provided for by the user
     1 (using  "SPACE", "LUNIT4"),  if it, too,'  /
      DATA  TEXSPY ( 251 )   /  '  is to be permanently saved.          
     1                                         '  /
      DATA  TEXSPY ( 252 )   /  'KEY WORD NO. 25:  "SOURCE"      ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 253 )   /  '  This response to the  "SPY:"  prompt 
     1will allow the user to look at either    '  /
      DATA  TEXSPY ( 254 )   /  '  the electric network or the TACS sour
     1ce table.   There will be a pause after  '  /
      DATA  TEXSPY ( 255 )   /  '  spy receives  "SOURCE",  as it waits 
     1to receive either  "TACS"  or  "ELEC"  as'  /
      DATA  TEXSPY ( 256 )   /  '  an indication of source-table choice.
     1   Then  SPY  waits for a pair of free-  '  /
      DATA  TEXSPY ( 257 )   /  '  format integer row numbers, to define
     1 the limits of the table display.   A few'  /
      DATA  TEXSPY ( 258 )   /  '  key words are also accepted here:  "A
     1LL"  to display entire table,  "END"  or '  /
      DATA  TEXSPY ( 259 )   /  '  "STOP"  or  "SPY"  to return to  "SPY
     1:"  prompt,  and  "TACS"  or  "ELEC"  to '  /
      DATA  TEXSPY ( 260 )   /  '  produce a new table heading (or switc
     1h between the two tables).   The TACS    '  /
      DATA  TEXSPY ( 261 )   /  '  table displays offsets of  "SPTACS"  
     1which are used with  "DEPOSIT/EXAMINE",  '  /
      DATA  TEXSPY ( 262 )   /  '  in case the user wants to redefine su
     1ch quantities.                           '  /
      DATA  TEXSPY ( 263 )   /  'KEY WORD NO. 26:  "EDIT"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 264 )   /  '  This response to the  "SPY:"  prompt 
     1will allow the user to examine and modify'  /
      DATA  TEXSPY ( 265 )   /  '  card images which are currently store
     1d in memory (see the  "DATA"  command).  '  /
      DATA  TEXSPY ( 266 )   /  '  A  "*"  prompt will next appear, at w
     1hich point VAX SOS-like editing commands '  /
      DATA  TEXSPY ( 267 )   /  '  can be issued.  The only bothersome c
     1hanges of notation are the use of  "#"   '  /
      DATA  TEXSPY ( 268 )   /  '  in place of VAX"s  "!"  (because of i
     1n-line comment problems with E/Ts),  and '  /
      DATA  TEXSPY ( 269 )   /  '  use of  "@"  in place of VAX"s  "ESC"
     1  key as a character string delimiter.   '  /
      DATA  TEXSPY ( 270 )   /  '  For an explanation of SOS editing rul
     1es, see DEC VAX-11 user documentation.   '  /
      DATA  TEXSPY ( 271 )   /  '  Examples of printing include:  "*P^:*
     1",  "*P5",  "*P^:18",  "*P5#10",  "*P",  '  /
      DATA  TEXSPY ( 272 )   /  '  and  "P.-20:."   There also is  "*F" 
     1usage,  only with  "@"  replacing  <ESC> '  /
      DATA  TEXSPY ( 273 )   /  '  of  VAX/VMS  SOS.  To exit the  "EDIT
     1"  command and return to the  "SPY:"     '  /
      DATA  TEXSPY ( 274 )   /  '  prompt, use  "*E"  (analogous to the 
     1SOS exit).  Additional user commands     '  /
      DATA  TEXSPY ( 275 )   /  '  include  "*D"  (for deletion of lines
     1),  "*R"  (for replacement of lines),    '  /
      DATA  TEXSPY ( 276 )   /  '  "*I"  (for insertion of new lines),  
     1and  "*S"  (for the substitution of one  '  /
      DATA  TEXSPY ( 277 )   /  '  character string by another).  Concer
     1ning  "*S",  however, once again  "@"    '  /
      DATA  TEXSPY ( 278 )   /  '  is used as a delimiter rather than  <
     1ESC>,  and no qualifiers (e.g., ",D" for '  /
      DATA  TEXSPY ( 279 )   /  '  "decide mode") are allowed.   Finally
     1, there are special EMTP-designed        '  /
      DATA  TEXSPY ( 280 )   /  '  commands.   The first of these is  "*
     18",  which initiates a search for the    '  /
      DATA  TEXSPY ( 281 )   /  '  next card which has a non-blank colum
     1n 80.   After display of this record,    '  /
      DATA  TEXSPY ( 282 )   /  '  SPY awaits a user decision regarding 
     1disposition:  <CR>  will leave the card  '  /
      DATA  TEXSPY ( 283 )   /  '  unchanged and initiate a search for t
     1he following one;  digits 0, 1, 2, 3, 4  '  /
      DATA  TEXSPY ( 284 )   /  '  will result in the punching of this v
     1alue into column 80 before searching for '  /
      DATA  TEXSPY ( 285 )   /  '  the next such record (with "0" intern
     1ally changed to a blank before punching).'  /
      DATA  TEXSPY ( 286 )   /  '  The  "*"  prompt will reappear automa
     1tically when the search hits the bottom  '  /
      DATA  TEXSPY ( 287 )   /  '  of the file.  Or,  "*"  can be reache
     1d at any point of the search-display loop'  /
      DATA  TEXSPY ( 288 )   /  '  by sending  "E" .   Even if not in th
     1e  "*8"  loop, column-80 deposits are    '  /
      DATA  TEXSPY ( 289 )   /  '  possible by use of  "*8,?"  where  "?
     1"  is the desired col.-80 content of the '  /
      DATA  TEXSPY ( 290 )   /  '  current line.   Finally,  the command
     1  "*COL"  will produce a heading of      '  /
      DATA  TEXSPY ( 291 )   /  '  column numbers, which is useful when 
     1inserting new data records using  "*I" . '  /
      DATA  TEXSPY ( 292 )   /  '  This ruler heading will be shifted to
     1 line up with  "*P"  displays.   If an   '  /
      DATA  TEXSPY ( 293 )   /  '  unshifted display is desired,  use  "
     1*COL8" .                                 '  /
      DATA  TEXSPY ( 294 )   /  'KEY WORD NO. 27:  "WAKE"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 295 )   /  '  This response to the  "SPY:"  prompt 
     1will awaken a hibernating solution (one  '  /
      DATA  TEXSPY ( 296 )   /  '  which was put to bed with an earlier 
     1 "SLEEP"  command).   Altered program    '  /
      DATA  TEXSPY ( 297 )   /  '  dimensions are not allowed  (both pro
     1grams must be dimensioned identically),  '  /
      DATA  TEXSPY ( 298 )   /  '  just as with the batch-mode  "START A
     1GAIN".  The  "WAKE"  command is the exact'  /
      DATA  TEXSPY ( 299 )   /  '  interactive equivalent of the batch-m
     1ode request  "START AGAIN".  There is a  '  /
      DATA  TEXSPY ( 300 )   /  '  related  "WAKE4"  command for regener
     1ation of the LUNIT4 plot-file header     '  /
      DATA  TEXSPY ( 301 )   /  '  information, in case this is wanted b
     1y the user (it is in fact needed, if the '  /
      DATA  TEXSPY ( 302 )   /  '  "PLOT"  command is to be used, as of 
     126 Feb 1984).                            '  /
      DATA  TEXSPY ( 303 )   /  'KEY WORD NO. 28:  "LANGUAGE"    ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 304 )   /  '  This response to the  "SPY:"  prompt 
     1allows the user to either examine or     '  /
      DATA  TEXSPY ( 305 )   /  '  modify "SPY:"-level command words (e.
     1g., "PLOT", "DATA", etc.).  A loop will  '  /
      DATA  TEXSPY ( 306 )   /  '  be entered, in which there are only 4
     1 legal responses  ("SINGLE",  "ENTIRE",  '  /
      DATA  TEXSPY ( 307 )   /  '  "SHOW",  and  "SPY")  to the prompt. 
     1 Sending  "SINGLE"  will lead to an      '  /
      DATA  TEXSPY ( 308 )   /  '  inner input loop in which old and new
     1 symbol pairs are to be redefined one at '  /
      DATA  TEXSPY ( 309 )   /  '  a time, terminated by  "END"  (to ret
     1urn to outer  "LANGUAGE"  loop) or  "SPY"'  /
      DATA  TEXSPY ( 310 )   /  '  (to return to the  "SPY:"  prompt).  
     1 The second outer response,  "ENTIRE",   '  /
      DATA  TEXSPY ( 311 )   /  '  must be followed by an unabridged dic
     1tionary of symbols using 10A8 format.    '  /
      DATA  TEXSPY ( 312 )   /  '  Sending the third response  "SHOW"  w
     1ill result in an unabridged display of   '  /
      DATA  TEXSPY ( 313 )   /  '  all such current commands.  Finally, 
     1sending  "SPY"  will exit the outer loop,'  /
      DATA  TEXSPY ( 314 )   /  '  and return to the  "SPY:"  prompt.  T
     1he language of  "PLOT"  usage does not   '  /
      DATA  TEXSPY ( 315 )   /  '  occur at the command level with promp
     1t  "SPY:",  so it can only be redefined  '  /
      DATA  TEXSPY ( 316 )   /  '  within that utility (by means of the 
     1 "SET DATA"  command).                   '  /
      DATA  TEXSPY ( 317 )   /  'KEY WORD NO. 29:  "CATALOG"     ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 318 )   /  '  This response to the  "SPY:"  prompt 
     1will create a new permanent disk file, it'  /
      DATA  TEXSPY ( 319 )   /  '  will dump a copy of the data case whi
     1ch is presently contained within the     '  /
      DATA  TEXSPY ( 320 )   /  '  EMTP into that disk file.   The name 
     1of this new disk file is user-supplied   '  /
      DATA  TEXSPY ( 321 )   /  '  (in response to a subsequent prompt f
     1or such a name, which must, naturally, be'  /
      DATA  TEXSPY ( 322 )   /  '  legal for the computer system being u
     1sed).   The EMTP data case in question   '  /
      DATA  TEXSPY ( 323 )   /  '  generally will differ from that origi
     1nally read in using the  "DATA"  command,'  /
      DATA  TEXSPY ( 324 )   /  '  of course  (assuming  "EDIT"  operati
     1ons have produced alterations).          '  /
      DATA  TEXSPY ( 325 )   /  'KEY WORD NO. 30:  "BEGIN"       ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 326 )   /  '  This response to the  "SPY:"  prompt 
     1will abort the current EMTP solution,    '  /
      DATA  TEXSPY ( 327 )   /  '  and initiate a complete new solution.
     1   It is the simulator-stored EMTP data  '  /
      DATA  TEXSPY ( 328 )   /  '  cards which are used as input for the
     1 new solution, and these will usually    '  /
      DATA  TEXSPY ( 329 )   /  '  have just been modified using  "EDIT"
     1  operations.  The transition is         '  /
      DATA  TEXSPY ( 330 )   /  '  "instantaneous" for cases which have 
     1been put to bed via  "REST"  or  "SLEEP",'  /
      DATA  TEXSPY ( 331 )   /  '  or which are cycling the time-step lo
     1op.   For execution in earlier overlays, '  /
      DATA  TEXSPY ( 332 )   /  '  there may be a delay until the presen
     1t overlay is exited.                     '  /
      DATA  TEXSPY ( 333 )   /  'KEY WORD NO. 31:  "STEP"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 334 )   /  '  This response to the  "SPY:"  prompt 
     1represents a request to toggle the binary'  /
      DATA  TEXSPY ( 335 )   /  '  switch that  forces  a  SPY  interrup
     1t at each and every possible opportunity.'  /
      DATA  TEXSPY ( 336 )   /  '  Either  there  are  such  ever-presen
     1t forced interrupts, or  there  are none.'  /
      DATA  TEXSPY ( 337 )   /  '  Interrupt opportunities exist at the 
     1start of each overlay,  at  the start  of'  /
      DATA  TEXSPY ( 338 )   /  '  each of the four pieces of overlay 16
     1  (the time-step loop),  and  finally, as'  /
      DATA  TEXSPY ( 339 )   /  '  each new data card is read by input m
     1odule  "CIMAGE".                         '  /
      DATA  TEXSPY ( 340 )   /  'KEY WORD NO. 32:  "DEBUG"       ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 341 )   /  '  This response to the  "SPY:"  prompt 
     1is used only for debugging of interactive'  /
      DATA  TEXSPY ( 342 )   /  '  EMTP execution itself, in case of fau
     1lty operation.   It is not used for cases'  /
      DATA  TEXSPY ( 343 )   /  '  of suspected EMTP error (do not confu
     1se with  "IPRSUP"  of the EMTP).  Anyway,'  /
      DATA  TEXSPY ( 344 )   /  '  there will be a subsequent prompt for
     1 diagnostic control variable  IPRSPY.    '  /
      DATA  TEXSPY ( 345 )   /  'KEY WORD NO. 33:  "DATA"       ----  --
     1--  ----                                 '  /
      DATA  TEXSPY ( 346 )   /  '  This response to the  "SPY:"  prompt 
     1is appropriate when the user wants to    '  /
      DATA  TEXSPY ( 347 )   /  '  specify a disk file of EMTP data whic
     1h is to be solved next.  The program     '  /
      DATA  TEXSPY ( 348 )   /  '  will then prompt for the desired file
     1 name, which will generally be computer- '  /
      DATA  TEXSPY ( 349 )   /  '  dependent.  A full, legal file name f
     1or the computer being used must then be  '  /
      DATA  TEXSPY ( 350 )   /  '  provided.  Before giving the real fil
     1e name, however, the user could send     '  /
      DATA  TEXSPY ( 351 )   /  '  "CONTROL",  should he want to input o
     1nly a portion of the disk file, or should'  /
      DATA  TEXSPY ( 352 )   /  '  it be desired to have the data placed
     1 in the EMTP cache with an offset, or    '  /
      DATA  TEXSPY ( 353 )   /  '  should it be desired to have the  LUN
     1IT5  usage pointer set to other than the '  /
      DATA  TEXSPY ( 354 )   /  '  default value of unity (offset zero).
     1  A second non-file response is  "SPY",  '  /
      DATA  TEXSPY ( 355 )   /  '  to abort the file-name prompt after t
     1he  "CONTROL"  definitions (in case the  '  /
      DATA  TEXSPY ( 356 )   /  '  user only wants to redefine the  LUNI
     1T5  usage pointer, for example).   A     '  /
      DATA  TEXSPY ( 357 )   /  '  final point concerns the avoidance of
     1 non-existent files (which would result  '  /
      DATA  TEXSPY ( 358 )   /  '  in an error stop of the EMTP by the o
     1perating system).  If in doubt about a   '  /
      DATA  TEXSPY ( 359 )   /  '  file"s existence, try to  TYPE  it fi
     1rst (the  "TYPE"  command warns of non-  '  /
      DATA  TEXSPY ( 360 )   /  '  existent files, without any terminati
     1on of execution).                        '  /
      DATA  TEXSPY ( 361 )   /  'KEY WORD NO. 34:  "RAMP"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 362 )   /  '  This response to the  "SPY:"  prompt 
     1will result in prompts which allow the   '  /
      DATA  TEXSPY ( 363 )   /  '  user to modify any variables in EMTP 
     1COMMON blocks as linear functions of     '  /
      DATA  TEXSPY ( 364 )   /  '  time.   That is, the the user can ram
     1p the values between beginning and ending'  /
      DATA  TEXSPY ( 365 )   /  '  limits,  as EMTP simulation time move
     1s between beginning and ending times --- '  /
      DATA  TEXSPY ( 366 )   /  '  with all control parameters being use
     1r-defined.  Any variable which can be    '  /
      DATA  TEXSPY ( 367 )   /  '  seen via the  "EXAMINE"  command can 
     1also be  RAMPed.  In specifying parameter'  /
      DATA  TEXSPY ( 368 )   /  '  values, there are three nested loops.
     1   The outer loop is for time, the middle'  /
      DATA  TEXSPY ( 369 )   /  '  one is for variable values, and the i
     1nner one is for variable names.   The    '  /
      DATA  TEXSPY ( 370 )   /  '  user can stay inside (middle or inner
     1 loops) as long as the outer quantity is '  /
      DATA  TEXSPY ( 371 )   /  '  not altered.   "END" will exit any on
     1e level,  and head outward to the next   '  /
      DATA  TEXSPY ( 372 )   /  '  one.   At the outer-most level, the o
     1ptional  "SHOW"  command will display all'  /
      DATA  TEXSPY ( 373 )   /  '  ramps which the user has defined thus
     1 far,  while  "END"  or  "SPY"  will     '  /
      DATA  TEXSPY ( 374 )   /  '  return to the  "SPY:"  prompt.  Actua
     1l variable redefinition is handled within'  /
      DATA  TEXSPY ( 375 )   /  '  module  "ANALYT"  of overlay 20 of th
     1e EMTP, where ramping logic has been     '  /
      DATA  TEXSPY ( 376 )   /  '  built (this is automatic, as the simu
     1lation progresses).                      '  /
      DATA  TEXSPY ( 377 )   /  'KEY WORD NO. 35:  "TIME"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 378 )   /  '  This response to the  "SPY:"  prompt 
     1 will result in the display of  both  the'  /
      DATA  TEXSPY ( 379 )   /  '  wall-clock time  (the date, too)  and
     1  also  the  current EMTP simulation time'  /
      DATA  TEXSPY ( 380 )   /  '  parameters (T, TMAX, DELTAT).        
     1                                         '  /
      DATA  TEXSPY ( 381 )   /  'KEY WORD NO. 36:  "TEK"         ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 382 )   /  '  This response to the  "SPY:"  prompt 
     1allows modification to an ongoing vector-'  /
      DATA  TEXSPY ( 383 )   /  '  graphic display which is currently  R
     1OLLing.  Yes, the user could simply      '  /
      DATA  TEXSPY ( 384 )   /  '  regenerate the plot from the beginnin
     1g (by use of a  "PLOT"  command following'  /
      DATA  TEXSPY ( 385 )   /  '  a user-keyed interrupt), but minor ch
     1anges can often be made more easily, and '  /
      DATA  TEXSPY ( 386 )   /  '  with less disruption, using  "TEK".  
     1 A subsequent prompt will then list a    '  /
      DATA  TEXSPY ( 387 )   /  '  menu of alternatives which are availa
     1ble for such "on-the-fly"  ROLLing       '  /
      DATA  TEXSPY ( 388 )   /  '  "PLOT"  tampering.  For completeness,
     1 that menu is repeated here:             '  /
      DATA  TEXSPY ( 389 )   /  '       SPY:TEK                         
     1                                         '  /
      DATA  TEXSPY ( 390 )   /  '        >< To tamper with the ROLLing v
     1ector plot,  send choice.                '  /
      DATA  TEXSPY ( 391 )   /  '        >< OPTION (MARK, DELAY, INNER, 
     1OVERLAP, END, HELP) :                    '  /
      DATA  TEXSPY ( 392 )   /  '           MARK  ---- for instantaneous
     1 marking of curves on Tek screen;        '  /
      DATA  TEXSPY ( 393 )   /  '           DELAY  --- to control how si
     1multaneous the ROLLing is to be;         '  /
      DATA  TEXSPY ( 394 )   /  '           INNER  ---- to  CALL TIMVAL 
     1 (the "INNER:" level of PLOTing);        '  /
      DATA  TEXSPY ( 395 )   /  '           OVERLAP  -- to modify the pe
     1rcent overlap for new-page plot;         '  /
      DATA  TEXSPY ( 396 )   /  '           END   ---- for return to  "S
     1PY:"  prompt.                            '  /
      DATA  TEXSPY ( 397 )   /  '  Unless the user requests curve identi
     1fication via  "MARK",  the  ROLLing plot '  /
      DATA  TEXSPY ( 398 )   /  '  will only have it for the regenerated
     1 portion (the overlap from the previous  '  /
      DATA  TEXSPY ( 399 )   /  '  page, as controlled by  "OVERLAP").  
     1 The  "DELAY"  command allows the user   '  /
      DATA  TEXSPY ( 400 )   /  '  to control the time-step multiplicity
     1 between plotting sessions.  There is a  '  /
      DATA  TEXSPY ( 401 )   /  '  trade off between solution efficiency
     1 (maximized if plotting is infrequent)   '  /
      DATA  TEXSPY ( 402 )   /  '  and simultaneous observation (maximiz
     1ed if plotting occurs on each new time   '  /
      DATA  TEXSPY ( 403 )   /  '  step).   So much for the concept.  Bu
     1t the details of counting are in fact a  '  /
      DATA  TEXSPY ( 404 )   /  '  little different, since it is not tim
     1e steps, but rather plot points, which   '  /
      DATA  TEXSPY ( 405 )   /  '  are counted.   Only if every solution
     1 point becomes a plot point (only if the '  /
      DATA  TEXSPY ( 406 )   /  '  miscellaneous data parameter  IPLOT  
     1is equal to unity) are these two equal,  '  /
      DATA  TEXSPY ( 407 )   /  '  note.  So, if  IOUT = 3  and the user
     1 wants to see plotting progress about    '  /
      DATA  TEXSPY ( 408 )   /  '  every twentieth time-step, he would s
     1end  "7"  in response to the prompt for a'  /
      DATA  TEXSPY ( 409 )   /  '  multiplicity after his  "DELAY"  was 
     1accepted.   The result would then be     '  /
      DATA  TEXSPY ( 410 )   /  '  incremental plotting every 21st step 
     1(21 = 3*7).   Concerning  "OVERLAP",  it '  /
      DATA  TEXSPY ( 411 )   /  '  is generally recommended that this on
     1ly be used for terminals without memory, '  /
      DATA  TEXSPY ( 412 )   /  '  since any percentage greater than zer
     1o does increase the plotting burden.   In'  /
      DATA  TEXSPY ( 413 )   /  '  the case of displays with near-infini
     1te storage (e.g., Apollo windows),  it is'  /
      DATA  TEXSPY ( 414 )   /  '  better to leave the overlap at the de
     1fault value of zero.   Use of  "INNER"   '  /
      DATA  TEXSPY ( 415 )   /  '  both very powerful and potentially ve
     1ry tricky, since it results in a  CALL to'  /
      DATA  TEXSPY ( 416 )   /  '  SUBROUTINE TIMVAL,  which is responsi
     1ble for the  "INNER:"  level of  "PLOT"  '  /
      DATA  TEXSPY ( 417 )   /  '  dialogue.   All  "INNER:"  level defi
     1nitions are therefore available to the   '  /
      DATA  TEXSPY ( 418 )   /  '  user, although effects may or may not
     1 be as expected (either trial and error  '  /
      DATA  TEXSPY ( 419 )   /  '  experience, or understanding of "TPPL
     1OT" FORTRAN, are required in order to    '  /
      DATA  TEXSPY ( 420 )   /  '  predict the results of any given oper
     1ation).   The one big difference is that '  /
      DATA  TEXSPY ( 421 )   /  '  the use of  "TIME"  or just  <CR>  do
     1 not result in the production of a new   '  /
      DATA  TEXSPY ( 422 )   /  '  plot.  To return back to the  "TEK"  
     1prompt from the  "INNER:"  prompt,       '  /
      DATA  TEXSPY ( 423 )   /  '  send  "SPY".   As for return to the  
     1"SPY"  prompt,  either the listed  "END" '  /
      DATA  TEXSPY ( 424 )   /  '  or  "SPY"  will work.                
     1                                         '  /
      DATA  TEXSPY ( 425 )   /  'KEY WORD NO. 37:  "BRANCH"      ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 426 )   /  '  This response to the  "SPY:"  prompt 
     1 is  issued  for  a  display  of the EMTP'  /
      DATA  TEXSPY ( 427 )   /  '  branch table.  Subsequently send an a
     1dditional  "EXTRA"  to change to the next'  /
      DATA  TEXSPY ( 428 )   /  '  sub-table  (as of February 1984, ther
     1e  are  two),  if  different columns  are'  /
      DATA  TEXSPY ( 429 )   /  '  desired.  Within any one choice as to
     1 sub-table,  there is a loop in which the'  /
      DATA  TEXSPY ( 430 )   /  '  user sends a pair of free-format begi
     1nning and ending row numbers for display,'  /
      DATA  TEXSPY ( 431 )   /  '  or  a  key word.  Recognized key word
     1s here  are  "ALL"  for the entire table,'  /
      DATA  TEXSPY ( 432 )   /  '  "TOP"  for row one,  "BOT"  for the  
     1last  row,  "NEXT"  or  just  a  carriage'  /
      DATA  TEXSPY ( 433 )   /  '  return for the following row, and of 
     1course  "SPY"  to  abort the display loop'  /
      DATA  TEXSPY ( 434 )   /  '  and return to the  "SPY:"  prompt.  T
     1o refresh the heading, use either  "BRAN"'  /
      DATA  TEXSPY ( 435 )   /  '  or  "HEAD"  (only 4 characters of  "B
     1RANCH"  or  "HEADING"  are checked).     '  /
      DATA  TEXSPY ( 436 )   /  'KEY WORD NO. 38:  "YFORM"       ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 437 )   /  '  This  response  to  the  "SPY:"  prom
     1pt  will result in continuous (every time'  /
      DATA  TEXSPY ( 438 )   /  '  step)  re-formation of  [Y],  followe
     1d by re-triangularization.   It is turned'  /
      DATA  TEXSPY ( 439 )   /  '  off (no more [Y]-forming)  by sending
     1  "NOY"  after  "SPY:"  prompt.  ???  ???'  /
      DATA  TEXSPY ( 440 )   /  '  Warning:  like  the earlier batch-mod
     1e  "MODIFY DELTAT",  this looks better in'  /
      DATA  TEXSPY ( 441 )   /  '  theory than  it  does in practice.  F
     1or  important elements such as frequency-'  /
      DATA  TEXSPY ( 442 )   /  '  dependent transmission lines, the ori
     1ginal data which is needed to reform  [Y]'  /
      DATA  TEXSPY ( 443 )   /  '  has been destroyed by the solution, s
     1o the result  will  be erroneous.  Unless'  /
      DATA  TEXSPY ( 444 )   /  '  the user has a particularly degenerat
     1e problem,  and  he is sure that he knows'  /
      DATA  TEXSPY ( 445 )   /  '  what he is doing, the general recomme
     1ndation is to avoid all such use.  ??? ??'  /
      DATA  TEXSPY ( 446 )   /  'KEY WORD NO. 39:  "NOY"         ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 447 )   /  '  This response to the  "SPY:"  prompt 
     1will cancel a preceding  "YFORM"  request'  /
      DATA  TEXSPY ( 448 )   /  '  for continuous [Y]-formation.        
     1                                         '  /
      DATA  TEXSPY ( 449 )   /  'KEY WORD NO. 40:  "FACTOR"      ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 450 )   /  '  This  response  to  the  "SPY:"  prom
     1pt  will result in continuous (every time'  /
      DATA  TEXSPY ( 451 )   /  '  step) triangularization.   It is turn
     1ed off  (no more automatic factoring)  by'  /
      DATA  TEXSPY ( 452 )   /  '  sending  "NOF"  in response to the  "
     1SPY:"  prompt.                           '  /
      DATA  TEXSPY ( 453 )   /  'KEY WORD NO. 41:  "NOF"         ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 454 )   /  '  This  response  to  the  "SPY:"  prom
     1pt  will  cancel  a  preceding   "FACTOR"'  /
      DATA  TEXSPY ( 455 )   /  '  request for the continuous re-triangu
     1larization of [Y].                       '  /
      DATA  TEXSPY ( 456 )   /  'KEY WORD NO. 42:  "RLC"         ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 457 )   /  '  This response to the  "SPY:"  prompt 
     1will produce a display of the EMTP  R-L-C'  /
      DATA  TEXSPY ( 458 )   /  '  tables.  Subsequently  send  an  addi
     1tional  "EXTRA"  to  change to  the  next'  /
      DATA  TEXSPY ( 459 )   /  '  sub-table  (as of February 1984, ther
     1e  are  two),  if  different columns  are'  /
      DATA  TEXSPY ( 460 )   /  '  desired.  Within any one choice as to
     1 sub-table,  there is a loop in which the'  /
      DATA  TEXSPY ( 461 )   /  '  user sends a pair of free-format begi
     1nning and ending row numbers for display,'  /
      DATA  TEXSPY ( 462 )   /  '  or  a  key word.  Recognized key word
     1s here  are  "ALL"  for the entire table,'  /
      DATA  TEXSPY ( 463 )   /  '  "TOP"  for row one,  "BOT"  for the  
     1last  row,  "NEXT"  or  just  a  carriage'  /
      DATA  TEXSPY ( 464 )   /  '  return for the following row, and of 
     1course  "SPY"  to  abort the display loop'  /
      DATA  TEXSPY ( 465 )   /  '  and return to the  "SPY:"  prompt.  T
     1o refresh the heading,  use either  "RLC"'  /
      DATA  TEXSPY ( 466 )   /  '  or  "HEAD"  (only 4 characters of  "H
     1EADING"  are checked).                   '  /
      DATA  TEXSPY ( 467 )   /  'KEY WORD NO. 43:  "WIDTH"       ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 468 )   /  '  This response to the  "SPY:"  prompt 
     1will toggle the output line length  (if  '  /
      DATA  TEXSPY ( 469 )   /  '  equal to 132 at the time the command 
     1is issued, it will be changed to 80,  and'  /
      DATA  TEXSPY ( 470 )   /  '  vice-versa).  This is for EMTP line p
     1rinter output (channel LUNIT6) only.     '  /
      DATA  TEXSPY ( 471 )   /  'KEY WORD NO. 44:  "BUS"         ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 472 )   /  '  This response to the  "SPY:"  prompt 
     1will produce a display  of  the  EMTP bus'  /
      DATA  TEXSPY ( 473 )   /  '  vectors.   Subsequently  send  an  ad
     1ditional  "EXTRA"  to  change to the next'  /
      DATA  TEXSPY ( 474 )   /  '  next sub-table (as of February 1984, 
     1there are two),  if different columns are'  /
      DATA  TEXSPY ( 475 )   /  '  desired.  Within any one choice as to
     1 sub-table,  there is a loop in which the'  /
      DATA  TEXSPY ( 476 )   /  '  user sends a pair of free-format begi
     1nning and ending row numbers for display,'  /
      DATA  TEXSPY ( 477 )   /  '  or  a  key word.  Recognized key word
     1s here  are  "ALL"  for the entire table,'  /
      DATA  TEXSPY ( 478 )   /  '  "TOP"  for row one,  "BOT"  for the  
     1last  row,  "NEXT"  or  just  a  carriage'  /
      DATA  TEXSPY ( 479 )   /  '  return for the following row, and of 
     1course  "SPY"  to  abort the display loop'  /
      DATA  TEXSPY ( 480 )   /  '  and return to the  "SPY:"  prompt.  T
     1o refresh the heading,  use either  "BUS"'  /
      DATA  TEXSPY ( 481 )   /  '  or  "HEAD"  (only 4 characters of "HE
     1ADING" are checked).                     '  /
      DATA  TEXSPY ( 482 )   /  'KEY WORD NO. 45:  "SIZE"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 483 )   /  '  This response  to  the  "SPY:"  promp
     1t  will  produce a display of actual EMTP'  /
      DATA  TEXSPY ( 484 )   /  '  data requirements  ----  the  "PRESEN
     1T FIGURE"  List Sizes which are  seen  at'  /
      DATA  TEXSPY ( 485 )   /  '  the end of batch-mode EMTP printout, 
     1 in the case-summary statistics.         '  /
      DATA  TEXSPY ( 486 )   /  'KEY WORD NO. 46:  "LIMIT"       ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 487 )   /  '  This response  to  the  "SPY:"  promp
     1t  will produce a display of the limiting'  /
      DATA  TEXSPY ( 488 )   /  '  EMTP table sizes  ----  the  "PROGRAM
     1 LIMIT"  List Sizes which are seen at the'  /
      DATA  TEXSPY ( 489 )   /  '  end of batch-mode EMTP printout,  in 
     1the case-summary statistics.             '  /
      DATA  TEXSPY ( 490 )   /  'KEY WORD NO. 47:  "IOUT"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 491 )   /  '  This response  to  the  "SPY:"  promp
     1t  will alter the EMTP printout frequency'  /
      DATA  TEXSPY ( 492 )   /  '  according  to  the user"s latest desi
     1re.   The response will be instantaneous,'  /
      DATA  TEXSPY ( 493 )   /  '  with  a  later  minor adjustment at t
     1he first round step number  (the next one'  /
      DATA  TEXSPY ( 494 )   /  '  which  is  divisible  by  IOUT  with 
     1 zero  remainder).   This command cancels'  /
      DATA  TEXSPY ( 495 )   /  '  any  previous  batch-mode requests  f
     1or  modification  of the output frequency'  /
      DATA  TEXSPY ( 496 )   /  '  (e.g., the special-request word  "CHA
     1NGE PRINTOUT FREQUENCY").                '  /
      DATA  TEXSPY ( 497 )   /  'KEY WORD NO. 48:  "NODE"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 498 )   /  '  This response  to  the  "SPY:"  promp
     1t  will yield a question/answer loop with'  /
      DATA  TEXSPY ( 499 )   /  '  input being a user-supplied 6-charact
     1er bus name, and output (the SPY display)'  /
      DATA  TEXSPY ( 500 )   /  '  being the corresponding EMTP node num
     1ber.   After any one node number has been'  /
      DATA  TEXSPY ( 501 )   /  '  displayed,  the  user can send  "CONN
     1ECT"  to obtain a list of all row numbers'  /
      DATA  TEXSPY ( 502 )   /  '  of all connected branches/switches/no
     1nlinear elements.                        '  /
      DATA  TEXSPY ( 503 )   /  'KEY WORD NO. 49:  "NONLIN"      ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 504 )   /  '  This  response  to  the  "SPY:"  prom
     1pt  is  issued  for a display of the EMTP'  /
      DATA  TEXSPY ( 505 )   /  '  nonlinear element table.  Subsequentl
     1y  send an additional  "EXTRA"  to change'  /
      DATA  TEXSPY ( 506 )   /  '  to the next sub-table (as of February
     1 1984, there are just two),  if different'  /
      DATA  TEXSPY ( 507 )   /  '  columns are desired.  Within any one 
     1sub-table,  there is a loop in which  the'  /
      DATA  TEXSPY ( 508 )   /  '  user sends a pair of free-format begi
     1nning and ending row numbers for display,'  /
      DATA  TEXSPY ( 509 )   /  '  or  a  key word.  Recognized key word
     1s here  are  "ALL"  for the entire table,'  /
      DATA  TEXSPY ( 510 )   /  '  "TOP"  for row one,  "BOT"  for the  
     1last  row,  "NEXT"  or  just  a  carriage'  /
      DATA  TEXSPY ( 511 )   /  '  return for the following row, and of 
     1course  "SPY"  to  abort the display loop'  /
      DATA  TEXSPY ( 512 )   /  '  and return to the  "SPY:"  prompt.  T
     1o refresh the heading, use either  "NONL"'  /
      DATA  TEXSPY ( 513 )   /  '  or  "HEAD"  (only 4 characters of "NO
     1NLIN" or "HEADING" are checked).         '  /
      DATA  TEXSPY ( 514 )   /  'KEY WORD NO. 50:  "SPACE"       ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 515 )   /  '  This response to the  "SPY:"  prompt 
     1begins dialogue which can rearrange      '  /
      DATA  TEXSPY ( 516 )   /  '  storage of either data cards or plot 
     1data points.  Such use is mandatory if   '  /
      DATA  TEXSPY ( 517 )   /  '  no additional room for plot data rema
     1ins (logic of "PLTFIL" will deny passage '  /
      DATA  TEXSPY ( 518 )   /  '  until more storage has so been freed,
     1 in such a case).  The management of card'  /
      DATA  TEXSPY ( 519 )   /  '  images will generally be performed on
     1ly in conjunction with  "EDIT"  use, to  '  /
      DATA  TEXSPY ( 520 )   /  '  modify EMTP data.  Parameters related
     1 to these two storages will be displayed:'  /
      DATA  TEXSPY ( 521 )   /  '     INDBEG --- beginning location (ind
     1ex to /C29B01/) of plot data points;     '  /
      DATA  TEXSPY ( 522 )   /  '     INDBUF --- next free cell (index t
     1o /C29B01/) for storage of plot data;    '  /
      DATA  TEXSPY ( 523 )   /  '     LIMBUF --- limit on INDBUF (end of
     1 /C29B01/);                              '  /
      DATA  TEXSPY ( 524 )   /  '     NUMDCD --- last card read by EMTP 
     1(index to CHARACTER*80 FILE6 storage)    '  /
      DATA  TEXSPY ( 525 )   /  '     NUMCRD --- largest index for card 
     1storage in FILE6 (as read by DATA);      '  /
      DATA  TEXSPY ( 526 )   /  '     LIMCRD --- limiting index for FILE
     16 card storage (dimensioned limit).      '  /
      DATA  TEXSPY ( 527 )   /  '  Immediately after this display there 
     1will be a choice among the possible      '  /
      DATA  TEXSPY ( 528 )   /  '  responses  "CARDS",  "PLOT",  and  "S
     1PY".  The third of these will abort      '  /
      DATA  TEXSPY ( 529 )   /  '  the command, while the first two sele
     1ct between the two major classes of      '  /
      DATA  TEXSPY ( 530 )   /  '  SPACE management.                    
     1                                         '  /
      DATA  TEXSPY ( 531 )   /  '       (1) For the  "PLOT"  case,  the 
     1user must next choose among  "WRITE",    '  /
      DATA  TEXSPY ( 532 )   /  '  "THIN",  "DELETE",  and  "READ",  bas
     1ically.   The first of these writes plot '  /
      DATA  TEXSPY ( 533 )   /  '  points to disk just as a non-interact
     1ive program would, using I/O channel     '  /
      DATA  TEXSPY ( 534 )   /  '  number LUNIT4.   The only change is t
     1hat the time span  [Tbeg, Tend]  to be   '  /
      DATA  TEXSPY ( 535 )   /  '  dumped,  as well as the frequency of 
     1the output (the effective IPLOT), are    '  /
      DATA  TEXSPY ( 536 )   /  '  under interactive control (chosen by 
     1responses to subsequent prompts).  The   '  /
      DATA  TEXSPY ( 537 )   /  '  "THIN"  command discards points withi
     1n the  [Tbeg, Tend]  window according to '  /
      DATA  TEXSPY ( 538 )   /  '  a user-specified frequency.  The  "DE
     1LETE"  option destroys all points        '  /
      DATA  TEXSPY ( 539 )   /  '  (the effective IPLOT is infinite) wit
     1hin the user-specified time range        '  /
      DATA  TEXSPY ( 540 )   /  '  [Tbeg,  Tend].   The  "READ"  option 
     1is the reverse of  "WRITE",  allowing the'  /
      DATA  TEXSPY ( 541 )   /  '  restoration of LUNIT4 disk-stored poi
     1nts to the working interactive memory.   '  /
      DATA  TEXSPY ( 542 )   /  '  This is done with user-specified freq
     1uency within the time range [Tbeg, Tend].'  /
      DATA  TEXSPY ( 543 )   /  '  Finally, there is a hybrid command  "
     1FLUSH"  which combines  "WRITE"  and     '  /
      DATA  TEXSPY ( 544 )   /  '  "THIN"  (assuming that the user wants
     1 to use a common multiplicity for both   '  /
      DATA  TEXSPY ( 545 )   /  '  operations).  After compacting operat
     1ions, there will be printout of available'  /
      DATA  TEXSPY ( 546 )   /  '  free space.  The normal EMTP line pri
     1nter output (I/O channel LUNIT6) will    '  /
      DATA  TEXSPY ( 547 )   /  '  contain a record of the plot-file man
     1ipulations, to remind the user of what   '  /
      DATA  TEXSPY ( 548 )   /  '  he has done.  An extra command is  "A
     1UTO",  which gives an automatic, full    '  /
      DATA  TEXSPY ( 549 )   /  '  "FLUSH"  from that point onward.     
     1                                         '  /
      DATA  TEXSPY ( 550 )   /  '       (2) For the  CARDS  branch, the 
     1user is prompted to choose among  "MOVE",'  /
      DATA  TEXSPY ( 551 )   /  '  "COPY",  and  "BLANK".   The first of
     1 these is for block transfers (leaving   '  /
      DATA  TEXSPY ( 552 )   /  '  blanks in vacated locations), while t
     1he second is for block reproductions     '  /
      DATA  TEXSPY ( 553 )   /  '  (leaving the original locations unalt
     1ered).  The  "BLANK"  command is to      '  /
      DATA  TEXSPY ( 554 )   /  '  erase a block of cards.  In all three
     1 cases, beginning locations and total    '  /
      DATA  TEXSPY ( 555 )   /  '  number of cards processed are used to
     1 define the blocks involved.             '  /
      DATA  TEXSPY ( 556 )   /  '       Many sub-commands of the large  
     1"SPACE"  command allow the abort option  '  /
      DATA  TEXSPY ( 557 )   /  '  "SPY" (to return to the  "SPY:"  prom
     1pt), or the option of backing up one     '  /
      DATA  TEXSPY ( 558 )   /  '  level (via  "OUT").                  
     1                                         '  /
      DATA  TEXSPY ( 559 )   /  'KEY WORD NO. 51:  "LUNIT4"      ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 560 )   /  '  Issue  this  command  to  connect  (F
     1ORTRAN OPEN), disconnect (FORTRAN CLOSE),'  /
      DATA  TEXSPY ( 561 )   /  '  position,  or  inspect  the  contents
     1  of  the disk file of plot data which is'  /
      DATA  TEXSPY ( 562 )   /  '  connected  to  I/O unit number  LUNIT
     14.   Recall that a disk file can be given'  /
      DATA  TEXSPY ( 563 )   /  '  solution  points  via  the  "DISK"  o
     1r  "FLUSH"  subcommands  of  the  "SPACE"'  /
      DATA  TEXSPY ( 564 )   /  '  command.  Subcommand choices are as f
     1ollows: "OPEN",  "CLOSE",  "TOP",  "BOT",'  /
      DATA  TEXSPY ( 565 )   /  '  "NEXT",  "BACK",  and  "TIME".   In o
     1rder,  these shall now                   '  /
      DATA  TEXSPY ( 566 )   /  '  be summarized:                       
     1                                         '  /
      DATA  TEXSPY ( 567 )   /  '       OPEN -- subsequent prompts will 
     1allow for a user-supplied file name      '  /
      DATA  TEXSPY ( 568 )   /  '               (not to exceed 32 charac
     1ters), and the  "STATUS"  choice         '  /
      DATA  TEXSPY ( 569 )   /  '               between  "NEW"  and  "OL
     1D"  (vacuous or pre-defined files).      '  /
      DATA  TEXSPY ( 570 )   /  '       CLOSE -- There are no parameters
     1; the distinction between  "KEEP"        '  /
      DATA  TEXSPY ( 571 )   /  '                and  "DELETE"  is impli
     1cit (all OPENed files should be saved).  '  /
      DATA  TEXSPY ( 572 )   /  '       TOP  --- to position the LUNIT4 
     1disk file ready to read the beginning    '  /
      DATA  TEXSPY ( 573 )   /  '                plot data (for smallest
     1 time);                                  '  /
      DATA  TEXSPY ( 574 )   /  '       BOT  --- to position the LUNIT4 
     1disk file after reading the final plot   '  /
      DATA  TEXSPY ( 575 )   /  '                data (there is a READ u
     1ntil an end-of-file).  Certain computers '  /
      DATA  TEXSPY ( 576 )   /  '                (e.g., VAX) allow WRITE
     1ing at this point, so old plot files can '  /
      DATA  TEXSPY ( 577 )   /  '                be directly added to af
     1ter such an initial positioning.         '  /
      DATA  TEXSPY ( 578 )   /  '       NEXT --- to read and display the
     1 plot points of the next time instant.   '  /
      DATA  TEXSPY ( 579 )   /  '                This forward READ is fa
     1st for all known computers.              '  /
      DATA  TEXSPY ( 580 )   /  '       BACK --- to read and display plo
     1t points of the preceding time step.     '  /
      DATA  TEXSPY ( 581 )   /  '                Since this uses BACKSPA
     1CE, be aware that for some computers     '  /
      DATA  TEXSPY ( 582 )   /  '                (PRIME, Apollo, etc.), 
     1this command is internally converted to  '  /
      DATA  TEXSPY ( 583 )   /  '                a  "TOP"  and  "TIME"  
     1command which backs up two steps.        '  /
      DATA  TEXSPY ( 584 )   /  '       TIME --- to position the LUNIT4 
     1plot file immediately after having read  '  /
      DATA  TEXSPY ( 585 )   /  '                points for the first st
     1ep at or beyond the user-specified time; '  /
      DATA  TEXSPY ( 586 )   /  'KEY WORD NO. 52:  "SERIES"      ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 587 )   /  '  Issue this command in order to modify
     1 the values of series R-L-C branches     '  /
      DATA  TEXSPY ( 588 )   /  '  within the time-step loop.  Such usag
     1e requires one unused row of both the    '  /
      DATA  TEXSPY ( 589 )   /  '  branch table (List 2) and the branch-
     1parameter table (List 7) for each series '  /
      DATA  TEXSPY ( 590 )   /  '  R-L-C branch of interest.  The  "SERI
     1ES"  command is coupled to the  "RAMP"   '  /
      DATA  TEXSPY ( 591 )   /  '  command in that  "SERIES"  must be us
     1ed to select the branches of interest    '  /
      DATA  TEXSPY ( 592 )   /  '  or potential interest ahead of time, 
     1while it is  "RAMP"  that actually varies'  /
      DATA  TEXSPY ( 593 )   /  '  them when the time comes.  This would
     1 be for continuous variation (ramping),  '  /
      DATA  TEXSPY ( 594 )   /  '  which is the most common case of inte
     1rest.  For step changes (single-time     '  /
      DATA  TEXSPY ( 595 )   /  '  occurances),  "RAMP"  is not used.  I
     1n either case,  actual changes to [Y]    '  /
      DATA  TEXSPY ( 596 )   /  '  occur just prior to factoring of [Y] 
     1in  "SUBTS1"  of overlay 16.  Previously,'  /
      DATA  TEXSPY ( 597 )   /  '  the user must issue a SPY call for  "
     1SERIES"  ----   at any point prior to    '  /
      DATA  TEXSPY ( 598 )   /  '  overlay 12.  This is remembered, so t
     1hat the EMTP will later automatically    '  /
      DATA  TEXSPY ( 599 )   /  '  break in the middle of  "OVER12"  wit
     1h a message that this is the time to     '  /
      DATA  TEXSPY ( 600 )   /  '  define the table of series R-L-C bran
     1ches which might later be tampered with. '  /
      DATA  TEXSPY ( 601 )   /  '  At the automatic break in  "OVER12", 
     1 the user chooses among the commands     '  /
      DATA  TEXSPY ( 602 )   /  '  "SHOW",  "EXTRA",  "CHANGE",  "STEP",
     1  "REWIND",  and  "SPY".   "SHOW"  will  '  /
      DATA  TEXSPY ( 603 )   /  '  display the table of series R-L-C bra
     1nches which have thus far been considered'  /
      DATA  TEXSPY ( 604 )   /  '  for possible later change,  with  "EX
     1TRA"  yielding an extension to the       '  /
      DATA  TEXSPY ( 605 )   /  '  display (a second table,  giving star
     1ting and next values).   "CHANGE"  is the'  /
      DATA  TEXSPY ( 606 )   /  '  gateway to various further choices  (
     1"DATA",  "MOVE",  "BLANK",  "USE",       '  /
      DATA  TEXSPY ( 607 )   /  '  "VALUE",  "END",  and  "SPY")  for th
     1e definition and manipulation of the     '  /
      DATA  TEXSPY ( 608 )   /  '  table of  "SHOW".   Quickly summarizi
     1ng these,  "DATA"  is used to copy series'  /
      DATA  TEXSPY ( 609 )   /  '  R-L-C branches into the tamper table,
     1 while  "MOVE"  will copy from one row of'  /
      DATA  TEXSPY ( 610 )   /  '  the tamper table to another, and  "BL
     1ANK"  will erase any such row.   "USE"   '  /
      DATA  TEXSPY ( 611 )   /  '  allows the user to toggle the activit
     1y status of any entry in the tamper      '  /
      DATA  TEXSPY ( 612 )   /  '  table,  while  "VALUE"  allows modifi
     1cation of R-L-C parameters in case this  '  /
      DATA  TEXSPY ( 613 )   /  '  is to be done discontinuously.   "END
     1"  moves outward, back to the preceding  '  /
      DATA  TEXSPY ( 614 )   /  '  prompt,  while  "SPY"  aborts  "SHOW"
     1  and returns to the  "SPY:"  prompt.    '  /
      DATA  TEXSPY ( 615 )   /  '  This completes  "CHANGE"  usage.   Ne
     1xt is  "STEP",  which is the command for '  /
      DATA  TEXSPY ( 616 )   /  '  a manual, discontinuous change at the
     1 next available opportunity (within one  '  /
      DATA  TEXSPY ( 617 )   /  '  time step).   "REWIND"  will erase th
     1e tamper table completely, so it is only '  /
      DATA  TEXSPY ( 618 )   /  '  recommended if the existing table is 
     1unsalvageable.   For details of the      '  /
      DATA  TEXSPY ( 619 )   /  '  behind the ramping of series R-L-C el
     1ements, see Vladimir"s article in the    '  /
      DATA  TEXSPY ( 620 )   /  '  EMTP Newsletter, Vol. 4, No. 2, Novem
     1ber, 1983.                               '  /
      DATA  TEXSPY ( 621 )   /  'KEY WORD NO. 53:  "LOCK"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 622 )   /  '  Issue this command to disable time-sh
     1aring between  SPY  and the ongoing EMTP '  /
      DATA  TEXSPY ( 623 )   /  '  simulation.  Subsequent cancellation 
     1of the command is by  "GO".   Since this '  /
      DATA  TEXSPY ( 624 )   /  '  command is absolute, it can be repeat
     1ed any number of times.   Besides this   '  /
      DATA  TEXSPY ( 625 )   /  '  user-defined  "LOCK"  command,  there
     1 are hidden, internal ones, such as the  '  /
      DATA  TEXSPY ( 626 )   /  '  one by module  "PLTFIL"  when plot da
     1ta space has been exhausted.  But a word '  /
      DATA  TEXSPY ( 627 )   /  '  of caution:  all of this talk about t
     1ime-sharing has meaning only for those   '  /
      DATA  TEXSPY ( 628 )   /  '  computers which allow time-sharing (w
     1ith type-ahead that is not erased by the '  /
      DATA  TEXSPY ( 629 )   /  '  user-keyed interrupt).               
     1                                         '  /
      DATA  TEXSPY ( 630 )   /  'KEY WORD NO. 54:  "[Y]"         ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 631 )   /  '  Issue this command to display one or 
     1more rows of the nodal admittance matrix '  /
      DATA  TEXSPY ( 632 )   /  '  [Y] of the time-step loop.   A subseq
     1uent prompt will request the desired row,'  /
      DATA  TEXSPY ( 633 )   /  '  which may either be specified by bus 
     1name (assumed to begin with a letter), or'  /
      DATA  TEXSPY ( 634 )   /  '  by a node number.  Alternate response
     1s are  "SPY"  or  "END"  (to return to   '  /
      DATA  TEXSPY ( 635 )   /  '  the  "SPY:"  prompt),  "TOP"  (to dis
     1play the row for node 2),  "BOT"  (to    '  /
      DATA  TEXSPY ( 636 )   /  '  display the row for node  KPARTB),  a
     1nd  "NEXT"  or  nothing  (to display the '  /
      DATA  TEXSPY ( 637 )   /  '  following row).   Each nonzero term o
     1f the row will be displayed,  next to    '  /
      DATA  TEXSPY ( 638 )   /  '  the associated column number.   A min
     1us sign applied to the column number is  '  /
      DATA  TEXSPY ( 639 )   /  '  used to mark the largest column of th
     1e row.   In case separate  "EXAMINE"     '  /
      DATA  TEXSPY ( 640 )   /  '  usage might be contemplated,  KKS  po
     1ints to the  (KM, YKM)  pairs.           '  /
      DATA  TEXSPY ( 641 )   /  'KEY WORD NO. 55:  "[F]"         ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 642 )   /  '  Issue this command to display one or 
     1more rows of the table of factors (the   '  /
      DATA  TEXSPY ( 643 )   /  '  triangularized [Y] of the time-step l
     1oop).   Display options are generally    '  /
      DATA  TEXSPY ( 644 )   /  '  identical to the immediately-precedin
     1g  "[Y]"  command,  except that here     '  /
      DATA  TEXSPY ( 645 )   /  '  only the upper triangle exists,  a mi
     1nus sign marks the diagonal entry,  and  '  /
      DATA  TEXSPY ( 646 )   /  '  KK  is the pointer (which can be obse
     1rved  via the  "BUS"  command).          '  /
      DATA  TEXSPY ( 647 )   /  'KEY WORD NO. 55:  "NOROLL"      ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 648 )   /  '  Use this command to cancel  ROLLing  
     1character and/or vector plots.   These   '  /
      DATA  TEXSPY ( 649 )   /  '  began,  it should be recalled,  via  
     1"ROLLC"  and/or  "ROLLV"  commands       '  /
      DATA  TEXSPY ( 650 )   /  '  (independent of each other, and also 
     1independent of the "TEK" setting) at the '  /
      DATA  TEXSPY ( 651 )   /  '  "INNER:"  level of PLOTting.   The us
     1er-keyed interrupt will by itself stop   '  /
      DATA  TEXSPY ( 652 )   /  '  the rolling only if control is within
     1  "CHRPLT"  or  "TEKPLT"  plotting       '  /
      DATA  TEXSPY ( 653 )   /  '  modules when it is spotted.  Otherwis
     1e, this added manual request is needed.  '  /
      DATA  TEXSPY ( 654 )   /  '  The only alternative would be to go b
     1ack into the  "INNER:"  level of         '  /
      DATA  TEXSPY ( 655 )   /  '  PLOTting to toggle the appropriate sw
     1itch (via  "ROLLC"  and/or  "ROLLV"      '  /
      DATA  TEXSPY ( 656 )   /  '  requests).                           
     1                                         '  /
      DATA  TEXSPY ( 657 )   /  'KEY WORD NO. 57:  "OPEN"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 658 )   /  '  Issue this command to  OPEN  a format
     1ted disk file.  A loop will be entered in'  /
      DATA  TEXSPY ( 659 )   /  '  which there will be a prompt for the 
     1next I/O unit number, and the disk       '  /
      DATA  TEXSPY ( 660 )   /  '  file which is to be connected thereto
     1.  To exit the loop, send a blank line.  '  /
      DATA  TEXSPY ( 661 )   /  'KEY WORD NO. 58:  "CLOSE"       ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 662 )   /  '  Issue this command to  CLOSE  a disk 
     1file.  A loop will be entered in         '  /
      DATA  TEXSPY ( 663 )   /  '  which there will be a prompt for the 
     1next I/O unit number, and the  STATUS    '  /
      DATA  TEXSPY ( 664 )   /  '  (either  KEEP  or  DELETE  are ok).  
     1To exit the loop, send a blank line.     '  /
      DATA  TEXSPY ( 665 )   /  'KEY WORD NO. 59:  "SM"          ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 666 )   /  '  Issue this command to display the var
     1ious electrical, mechanical, and solution'  /
      DATA  TEXSPY ( 667 )   /  '  parameters of any Type-59 synchronous
     1 machine (S.M.).  Time-sharing by SPY    '  /
      DATA  TEXSPY ( 668 )   /  '  is automatically suspended during thi
     1s display, due to algorithmic limitations'  /
      DATA  TEXSPY ( 669 )   /  '  of the Type-59 model (examination is 
     1only possible as that machine is being   '  /
      DATA  TEXSPY ( 670 )   /  '  processed within SUBROUTINE UPDATE, w
     1hich is called by "SUBTS1" (the first    '  /
      DATA  TEXSPY ( 671 )   /  '  piece of the time-step loop.  The use
     1r can issue the  "SM"  request at any    '  /
      DATA  TEXSPY ( 672 )   /  '  time, but in fact the EMTP simulation
     1 will stop within the processing loop of '  /
      DATA  TEXSPY ( 673 )   /  '  "UPDATE", and allow the user to accep
     1t or reject (Y or N responses) each      '  /
      DATA  TEXSPY ( 674 )   /  '  machine in turn.  Actually, only  "N"
     1  vetos the opportunity (a simple <CR>   '  /
      DATA  TEXSPY ( 675 )   /  '  will accept the next machine).  Once 
     1considering any one Type-59 S.M., there  '  /
      DATA  TEXSPY ( 676 )   /  '  are subsequent choices as to which pa
     1rameters the user wants to see:  "ELEC"  '  /
      DATA  TEXSPY ( 677 )   /  '  for electrical parameters,  "MECH"  f
     1or mechanical parameters, etc.           '  /
      DATA  TEXSPY ( 678 )   /  '  ?????????   As of 26 Feb 84, command 
     1is not complete   ?????????????????????  '  /
      DATA  TEXSPY ( 679 )   /  'KEY WORD NO. 60:  "HONK"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 680 )   /  '  Issue this command to ring the termin
     1al bell (i.e., honk its horn).  There    '  /
      DATA  TEXSPY ( 681 )   /  '  will then be a prompt for an integer 
     1severity level, which should be between  '  /
      DATA  TEXSPY ( 682 )   /  '  one and ten (zero produces no respons
     1e, while 10 indicates a disaster).       '  /
      DATA  TEXSPY ( 683 )   /  'KEY WORD NO. 61:  "CHOICE"      ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 684 )   /  '  Issue this command to see output vari
     1able names of any of the five classes    '  /
      DATA  TEXSPY ( 685 )   /  '  making up the EMTP output vector (1=n
     1ode voltages,  2=branch voltages,  etc.).'  /
      DATA  TEXSPY ( 686 )   /  '  Order of the five classes corresponds
     1 to the order of concatenation in the    '  /
      DATA  TEXSPY ( 687 )   /  '  EMTP output vector;  and within each 
     1class, the order corresponds, too.  The  '  /
      DATA  TEXSPY ( 688 )   /  '  display is dynamic,  so it always ref
     1lects the current output variables       '  /
      DATA  TEXSPY ( 689 )   /  '  (unlike  "CHOICE"  at the  "MIDDLE:" 
     1 level of PLOTting,  which only displays '  /
      DATA  TEXSPY ( 690 )   /  '  the original status of overlay 15).  
     1                                         '  /
      DATA  TEXSPY ( 691 )   /  'KEY WORD NO. 62:  "TACS"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 692 )   /  '  Issue this command as the gateway to 
     1concurrent sequential processing (CSP),  '  /
      DATA  TEXSPY ( 693 )   /  '  which is based on special, SPY-define
     1d TACS supplemental variable usage.  Then'  /
      DATA  TEXSPY ( 694 )   /  '  one enters a loop over CSP control co
     1mmands.   If  "REWIND"  is requested,    '  /
      DATA  TEXSPY ( 695 )   /  '  all previous definitions are erased, 
     1so definitions begin from level zero.  If'  /
      DATA  TEXSPY ( 696 )   /  '  "SOURCE"  is sent,  all following inp
     1ut is assumed to be TACS source cards,   '  /
      DATA  TEXSPY ( 697 )   /  '  until terminated by an  "END"  card. 
     1  If  "SUPPLEMENTAL"  is sent,  then all '  /
      DATA  TEXSPY ( 698 )   /  '  following data cards (SPY input) is a
     1ssumed to be TACS supplemental variable/ '  /
      DATA  TEXSPY ( 699 )   /  '  device data cards,  also until an  "E
     1ND"  card is encountered.  The subcommand'  /
      DATA  TEXSPY ( 700 )   /  '  "PATCH"  allows the user to connect a
     1ny variables to his TACS sources  (to    '  /
      DATA  TEXSPY ( 701 )   /  '  define the inputs),  and apply any su
     1pplemental variable results anywhere.    '  /
      DATA  TEXSPY ( 702 )   /  '  The "any" comes from the use of a mem
     1ory address of the SPY commands  "LIST"  '  /
      DATA  TEXSPY ( 703 )   /  '  or  "FIND",  just as  "EXAMINE"  or  
     1"DEPOSIT"  are quite unrestricted.  There'  /
      DATA  TEXSPY ( 704 )   /  '  will be separate prompts for all such
     1 TACS input and output connections.  When'  /
      DATA  TEXSPY ( 705 )   /  '  all such connections are complete,  "
     1SHOW"  can be used to display a summary  '  /
      DATA  TEXSPY ( 706 )   /  '  table of all TACS CSP input/output co
     1nnections.   Working room for such usage '  /
      DATA  TEXSPY ( 707 )   /  '  can be controlled by the user in seve
     1ral ways, if there are shortages.  First,'  /
      DATA  TEXSPY ( 708 )   /  '  if a user wants dummy output channels
     1 for printing/plotting, reserve dummy    '  /
      DATA  TEXSPY ( 709 )   /  '  node voltage outputs with  "CHAN01"  
     1punched in cols. 3-8, and the desired    '  /
      DATA  TEXSPY ( 710 )   /  '  number punched in columns 9-16 with I
     18 format.   Resultant names will be      '  /
      DATA  TEXSPY ( 711 )   /  '  serialized from the aforementioned ro
     1ot, and added to the output vector at the'  /
      DATA  TEXSPY ( 712 )   /  '  point of definition (if this card is 
     1first, all such dummy names will come    '  /
      DATA  TEXSPY ( 713 )   /  '  first, before other node voltages).  
     1Second, there is array dimensioning for  '  /
      DATA  TEXSPY ( 714 )   /  '  all vectors of the  "CHOICE"  table  
     1---  presently fixed in deck "DEKSPY", at'  /
      DATA  TEXSPY ( 715 )   /  '  the moment.  Finally, in addition to 
     1the familiar TACS table limits, there is '  /
      DATA  TEXSPY ( 716 )   /  '  an automatic reservation of space for
     1 all user-defined TACS CSP sources.  This'  /
      DATA  TEXSPY ( 717 )   /  '  is maximized automatically, within ex
     1isting TACS dimensions (as assigned by   '  /
      DATA  TEXSPY ( 718 )   /  '  ABSOLUTE TACS DIMENSIONS, for example
     1).  As usual,  "SPY"  will break out of  '  /
      DATA  TEXSPY ( 719 )   /  '  any loop, in case an abortion is desi
     1red.                                     '  /
      DATA  TEXSPY ( 720 )   /  'KEY WORD NO. 63:  "WAIT"        ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 721 )   /  '  The response to this SPY command will
     1 be a prompt for the desired delay       '  /
      DATA  TEXSPY ( 722 )   /  '  (hibernation) time in seconds.  Once 
     1this is accepted, the installation-      '  /
      DATA  TEXSPY ( 723 )   /  '  dependent SUBROUTINE TDELAY  is calle
     1d, to return control only after the      '  /
      DATA  TEXSPY ( 724 )   /  '  requested nap.                       
     1                                         '  /
      DATA  TEXSPY ( 725 )   /  'KEY WORD NO. 64:  "V-I""        ----  -
     1---  ----                                '  /
      DATA  TEXSPY ( 726 )   /  '  This SPY command is used to examine t
     1he List-10 characteristics of all        '  /
      DATA  TEXSPY ( 727 )   /  '  nonlinear and pseudo-nonlinear elemen
     1ts.  The name  "V-I"  is symbolic        '  /
      DATA  TEXSPY ( 728 )   /  '  only, since flux-current or resistanc
     1e-time characteristics are also          '  /
      DATA  TEXSPY ( 729 )   /  '  included.  Immediately upon entry, th
     1ere will be a display of the number      '  /
      DATA  TEXSPY ( 730 )   /  '  of nonlinear elements (List 9) and da
     1ta points of the characteristics         '  /
      DATA  TEXSPY ( 731 )   /  '  (List 10).  Also shown will be a list
     1 of options to a pair of free-format     '  /
      DATA  TEXSPY ( 732 )   /  '  row numbers:                         
     1                                         '  /
      DATA  TEXSPY ( 733 )   /  '     NEXT or <CR>  ---- to display the 
     1characteristic of the next element       '  /
      DATA  TEXSPY ( 734 )   /  '                        of the nonlinea
     1r element table.  There is wrap around   '  /
      DATA  TEXSPY ( 735 )   /  '                        from beginning 
     1to end.  No initialization required.     '  /
      DATA  TEXSPY ( 736 )   /  '     LAST  ---- to display the characte
     1ristic of the preceding element of       '  /
      DATA  TEXSPY ( 737 )   /  '                the nonlinear element t
     1able.  There is wrap around from the     '  /
      DATA  TEXSPY ( 738 )   /  '                beginning to the end.  
     1No initialization is required.           '  /
      DATA  TEXSPY ( 739 )   /  '     ALL  --- to display all rows of th
     1e List 10 characteristics.               '  /
      DATA  TEXSPY ( 740 )   /  '     SPY, END, or STOP ---- to abort di
     1splay loop, and return to  "SPY:"        '  /
      DATA  TEXSPY ( 741 )   /  '     MODE  ---- to toggle the binary sw
     1itch which selects between a display     '  /
      DATA  TEXSPY ( 742 )   /  '                by elements (value N8=0
     1), and a display by rows of List 10.     '  /
      DATA  TEXSPY ( 743 )   /  '                Initially, the display 
     1by elements, only one at a time, is      '  /
      DATA  TEXSPY ( 744 )   /  '                assumed.               
     1                                         '  /
      DATA  TEXSPY ( 745 )   /  '  In case the use inputs a pair of numb
     1ers instead of one of these key          '  /
      DATA  TEXSPY ( 746 )   /  '  words, then the response depends on  
     1MODE.   For the display by elements,     '  /
      DATA  TEXSPY ( 747 )   /  '  the first List-9 entry with a charact
     1eristic contained within the range       '  /
      DATA  TEXSPY ( 748 )   /  '  [N1, N2]  is identified, and displaye
     1d (where N1 and N2 are the ordered       '  /
      DATA  TEXSPY ( 749 )   /  '  pair of free-format numbers which the
     1 user has inputted.  On the other hand,  '  /
      DATA  TEXSPY ( 750 )   /  '  with  MODE  toggled for the absolute,
     1 unlimited display of List 10 rows,      '  /
      DATA  TEXSPY ( 751 )   /  '  then these rows (within limits of ava
     1ilability) are displayed.                '  /
      DATA  TEXSPY ( 752 )   /  '  The following plot commands are honor
     1ed at all three levels (either  "OUTER:",'  /
      DATA  TEXSPY ( 753 )   /  '  "MIDDLE",  or  "INNER:")  of plot dia
     1logue:                                   '  /
      DATA  TEXSPY ( 754 )   /  '  STOP  ---  To abort  "PLOT"  activity
     1,  and return to the  "SPY:"  prompt.    '  /
      DATA  TEXSPY ( 755 )   /  '  SPY   ---  To abort  "PLOT"  activity
     1,  and return to the  "SPY:"  prompt.    '  /
      DATA  TEXSPY ( 756 )   /  '  IN   ----  To transfer one level inwa
     1rd  (either from  "OUTER"  to  "MIDDLE:" '  /
      DATA  TEXSPY ( 757 )   /  '             or from  "MIDDLE"  to  "IN
     1NER")  in the plot dialogue.             '  /
      DATA  TEXSPY ( 758 )   /  '  HELP  ---  To produce the display now
     1 being viewed.  If unqualified, messages '  /
      DATA  TEXSPY ( 759 )   /  '             will be restricted to the 
     1level of current operation.  But one     '  /
      DATA  TEXSPY ( 760 )   /  '             blank and then a qualifier
     1 ("ALL", "OUTER", "MIDDLE", or "INNER")  '  /
      DATA  TEXSPY ( 761 )   /  '             provide the opportunity to
     1 see messages of other levels, too.      '  /
      DATA  TEXSPY ( 762 )   /  '  DEBUG  --  To alter the level of diag
     1nostic printout.  If none is now being   '  /
      DATA  TEXSPY ( 763 )   /  '             used, this command will tu
     1rn it on.  In fact, the control is       '  /
      DATA  TEXSPY ( 764 )   /  '             identical to that used in 
     1response to the  "SPY:"  prompt (IPRSPY).'  /
      DATA  TEXSPY ( 765 )   /  '               < < <  =======   END OF 
     1ANY-LEVEL  COMMANDS   =======  > > >     '  /
      DATA  TEXSPY ( 766 )   /  '  SET DATA -- To over-ride default data
     1 values with user-established choices.   '  /
      DATA  TEXSPY ( 767 )   /  '              The associated user-defin
     1ed data file must be pre-stored on disk, '  /
      DATA  TEXSPY ( 768 )   /  '              and must have the file na
     1me  "TPPARAM.DAT".                       '  /
      DATA  TEXSPY ( 769 )   /  '  TEK  ---- To toggle the switch which 
     1chooses between character plotting and   '  /
      DATA  TEXSPY ( 770 )   /  '            vector-graphic (e.g., Tektr
     1onix) plotting.  The default may be      '  /
      DATA  TEXSPY ( 771 )   /  '            computer dependent, or come
     1 from  "SET DATA"  usage.                '  /
      DATA  TEXSPY ( 772 )   /  '  COLUMN -- To toggle the switch that c
     1hooses between 80 and 132-column widths  '  /
      DATA  TEXSPY ( 773 )   /  '            for character plotting.    
     1                                         '  /
      DATA  TEXSPY ( 774 )   /  '  SET COLUMN -- To set character plot w
     1idths to any value.  But if either 80 or '  /
      DATA  TEXSPY ( 775 )   /  '                132 is desired (common 
     1choices), use the simpler  "COLUMN".     '  /
      DATA  TEXSPY ( 776 )   /  '               < < <  =======   END OF 
     1OUTER-LEVEL  COMMANDS   =======  > > >   '  /
      DATA  TEXSPY ( 777 )   /  '  TIMESPAN -- To be shown the time rang
     1e  [T-min, T-max]  of the plot data.     '  /
      DATA  TEXSPY ( 778 )   /  '  CHOICE -- To produce a tabulation of 
     1plotable EMTP variables.   But be very   '  /
      DATA  TEXSPY ( 779 )   /  '            careful, since the resultan
     1t tabulation corresponds to the status   '  /
      DATA  TEXSPY ( 780 )   /  '            before the time-step loop w
     1as entered.  For a dynamic tabulation,   '  /
      DATA  TEXSPY ( 781 )   /  '            use  "CHOICE"  in response 
     1to the  "SPY:"  prompt.                  '  /
      DATA  TEXSPY ( 782 )   /  '  TIME UNITS -- To specify the integer 
     1which characterizes the time units used  '  /
      DATA  TEXSPY ( 783 )   /  '                in the plotting.  This 
     1is just as with batch-mode EMTP plotting,'  /
      DATA  TEXSPY ( 784 )   /  '                where:  "1"  -- for deg
     1rees based on the power frequency;       '  /
      DATA  TEXSPY ( 785 )   /  '                        "2"  -- for cyc
     1les  based on the power frequency;       '  /
      DATA  TEXSPY ( 786 )   /  '                        "3,4,5"  -- for
     1 seconds, milliseconds, and microseconds '  /
      DATA  TEXSPY ( 787 )   /  '                        "6,7" -- for  "
     1FREQUENCY SCAN"  data cases, using either'  /
      DATA  TEXSPY ( 788 )   /  '                                 Hz or 
     1the log to base 10 of Hz,  rather than t.'  /
      DATA  TEXSPY ( 789 )   /  '  X   ----- Any garbage character (incl
     1uding blank) represents a request for the'  /
      DATA  TEXSPY ( 790 )   /  '            program to begin the interr
     1ogation for plot variables and labeling. '  /
      DATA  TEXSPY ( 791 )   /  '            Hereafter, in response to t
     1he different prompts, several special    '  /
      DATA  TEXSPY ( 792 )   /  '            key words are applicable (i
     1mmediately below).  Upon the completion  '  /
      DATA  TEXSPY ( 793 )   /  '            of all such information, th
     1ere is an automatic transfer to the      '  /
      DATA  TEXSPY ( 794 )   /  '            "INNER:"  level of program 
     1dialogue.  But first, options are:       '  /
      DATA  TEXSPY ( 795 )   /  '    REPEAT >> To reuse all former plot 
     1labels, send  "REPEAT"  when first asked '  /
      DATA  TEXSPY ( 796 )   /  '              for label information (wh
     1ich begins with the super title).  For   '  /
      DATA  TEXSPY ( 797 )   /  '              the first plot, "former l
     1abels" are all blank (initialization).   '  /
      DATA  TEXSPY ( 798 )   /  '    BACK >>>> To abort input of plot va
     1riables, and return to "MIDDLE:" prompt. '  /
      DATA  TEXSPY ( 799 )   /  '    END  >>>> To terminate all indeterm
     1inate inputs.  Included are the three    '  /
      DATA  TEXSPY ( 800 )   /  '              classes of plot variables
     1, and lines of case-title text.          '  /
      DATA  TEXSPY ( 801 )   /  '    LAST >>>> To terminate all plot-var
     1iable input, and begin specifying labels.'  /
      DATA  TEXSPY ( 802 )   /  '    blank >>> Sending nothing (just a c
     1arriage return) will usually reuse the   '  /
      DATA  TEXSPY ( 803 )   /  '              old datum (e.g., node nam
     1e, or line of case-title text).  Old data'  /
      DATA  TEXSPY ( 804 )   /  '              are usually displayed wit
     1hin parentheses for this reason.         '  /
      DATA  TEXSPY ( 805 )   /  '    FLUSH >>> To rewind the pointer whi
     1ch counts the lines of case-title text.  '  /
      DATA  TEXSPY ( 806 )   /  '              The text then remains, to
     1 be accepted or overwritten line by line.'  /
      DATA  TEXSPY ( 807 )   /  '    PLAYBACK> To display the entire pre
     1sent case title.  This command is legal  '  /
      DATA  TEXSPY ( 808 )   /  '              at any point before the  
     1"END"  which freezes the case title.     '  /
      DATA  TEXSPY ( 809 )   /  '  LABEL  -- To skip around the plot-var
     1iable specification (see "X" above), and '  /
      DATA  TEXSPY ( 810 )   /  '            begin the input of plot lab
     1els.  Plot variables remain unchanged.   '  /
      DATA  TEXSPY ( 811 )   /  '               < < <  =======   END OF 
     1MIDDLE-LEVEL  COMMANDS   =======  > > >  '  /
      DATA  TEXSPY ( 812 )   /  '  EXTREMA -- To toggle the switch that 
     1decides whether or not variable extrema  '  /
      DATA  TEXSPY ( 813 )   /  '             of subsequent plots are to
     1 be displayed.  Such output precedes the '  /
      DATA  TEXSPY ( 814 )   /  '             associated plot (as does t
     1hat of  "LEVEL"  below).  The program    '  /
      DATA  TEXSPY ( 815 )   /  '             then pauses before drawing
     1 the graph, waiting for the user to send '  /
      DATA  TEXSPY ( 816 )   /  '             a blank.  If the user want
     1s to skip the plot and return to the     '  /
      DATA  TEXSPY ( 817 )   /  '             "MIDDLE:"  level of dialog
     1ue, send  "NO PLOT".                     '  /
      DATA  TEXSPY ( 818 )   /  '  LEVEL -- To toggle the switch that de
     1cides whether or not level-triggers for  '  /
      DATA  TEXSPY ( 819 )   /  '           variables are to be activate
     1d.  If such triggers are being turned on,'  /
      DATA  TEXSPY ( 820 )   /  '           the program will next reques
     1t a level vector.  The response is using '  /
      DATA  TEXSPY ( 821 )   /  '           free-format.  The same pause
     1 exists as with  "EXTREMA".              '  /
      DATA  TEXSPY ( 822 )   /  '  SMOOTH -- To change the tolerance whi
     1ch is used to discard plot points.       '  /
      DATA  TEXSPY ( 823 )   /  '  SIZE   -- To change the length of the
     1 time axis of character plots.           '  /
      DATA  TEXSPY ( 824 )   /  '  SHOW   -- To display the current valu
     1es of many important plot parameters.    '  /
      DATA  TEXSPY ( 825 )   /  '  FACTOR -- To specify a new vector of 
     1multiplicative scaling factors for plot  '  /
      DATA  TEXSPY ( 826 )   /  '            variables (the "A" of  Z = 
     1A*Y + B ).   Zero is taken to mean unity,'  /
      DATA  TEXSPY ( 827 )   /  '            and free-format is used.   
     1                                         '  /
      DATA  TEXSPY ( 828 )   /  '  OFFSET -- Like  "FACTOR",  only to sp
     1ecify the vector of constants  "B".      '  /
      DATA  TEXSPY ( 829 )   /  '  RESCALE-- To return to natural scalin
     1g (i.e.,  A=1.0 ,  B=0.0 ) for all plot  '  /
      DATA  TEXSPY ( 830 )   /  '            variables.  This is used to
     1 erase previous  "FACTOR"  or  "OFFSET". '  /
      DATA  TEXSPY ( 831 )   /  '  LIMITS -- To specify new minimum and 
     1maximum values for the vertical axis of  '  /
      DATA  TEXSPY ( 832 )   /  '            plots.  Send  "0,0"  to can
     1cel previous manually-specified limits,  '  /
      DATA  TEXSPY ( 833 )   /  '            and return to the automatic
     1 scaling of the computer.                '  /
      DATA  TEXSPY ( 834 )   /  '  AVERAGE-- To change the limit on the 
     1number of consecutive oscillations after '  /
      DATA  TEXSPY ( 835 )   /  '            which the averaging of succ
     1essive ordinates is to be instituted.    '  /
      DATA  TEXSPY ( 836 )   /  '  TIME   -- To specify time-axis limits
     1  T-min  and  T-max  of the plot.  This  '  /
      DATA  TEXSPY ( 837 )   /  '            must be done at least once,
     1 for the first plot, unless  "TIMESPAN"  '  /
      DATA  TEXSPY ( 838 )   /  '            was issued at the  "MIDDLE:
     1"  level (resulting in full time range). '  /
      DATA  TEXSPY ( 839 )   /  '  ALL TIME--This is a request for a plo
     1t over the full time range of the data.  '  /
      DATA  TEXSPY ( 840 )   /  '            It functions correctly only
     1 if  "TIMESPAN"  was previously used at  '  /
      DATA  TEXSPY ( 841 )   /  '            the  "MIDDLE:"  level.     
     1                                         '  /
      DATA  TEXSPY ( 842 )   /  '  blank  -- A blank (just a <CR>) is in
     1terpreted as a request for a plot using  '  /
      DATA  TEXSPY ( 843 )   /  '            time-axis scaling as for th
     1e preceding plot (or the full range, if  '  /
      DATA  TEXSPY ( 844 )   /  '            no previous plots existed, 
     1but  "TIMESPAN"  was ordered).           '  /
      DATA  TEXSPY ( 845 )   /  '  CURSOR -- To toggle the switch that i
     1ndicates whether or not cursor input is  '  /
      DATA  TEXSPY ( 846 )   /  '            expected after the next vec
     1tor plot.  When the cursor is switched to'  /
      DATA  TEXSPY ( 847 )   /  '            "on",  following the plot t
     1here will be keyboard input following the'  /
      DATA  TEXSPY ( 848 )   /  '            positioning of the cursor: 
     1                                         '  /
      DATA  TEXSPY ( 849 )   /  '     P  >>>>> To mark another cursor po
     1int (wherever the cursor is sitting)     '  /
      DATA  TEXSPY ( 850 )   /  '     E  >>>>> To terminate such marking
     1 input (end of cursor use, actually).    '  /
      DATA  TEXSPY ( 851 )   /  '     SHOW >>> To produce a tabulation o
     1f all cursor points (previous "P" usage).'  /
      DATA  TEXSPY ( 852 )   /  '     SLOPE >> To produce  dx,  dy,  f, 
     1 and  dy/dx  of any pair of points that  '  /
      DATA  TEXSPY ( 853 )   /  '              the user is interested in
     1.  After the last such  (M,K)  pair of   '  /
      DATA  TEXSPY ( 854 )   /  '              point numbers which are o
     1f interest,  send  "0,0"  to terminate.  '  /
      DATA  TEXSPY ( 855 )   /  '     END  >>> To terminate all cursor d
     1isplays, and return to  "INNER:"  prompt.'  /
      DATA  TEXSPY ( 856 )   /  '  X-Y PLOT--To toggle the switch which 
     1chooses between regular plotting as a    '  /
      DATA  TEXSPY ( 857 )   /  '            function of time (the defau
     1lt), and X-Y plotting.                   '  /
      DATA  TEXSPY ( 858 )   /  '  ROLLV  -- To toggle the switch which 
     1produces a  ROLLing  vector plot.   It is'  /
      DATA  TEXSPY ( 859 )   /  '            normal to turn on such roll
     1ing after the basic plot is in place, and'  /
      DATA  TEXSPY ( 860 )   /  '            the axis scaling is judged 
     1to be appropriate.  Then, after  "ROLLV",'  /
      DATA  TEXSPY ( 861 )   /  '            the user will normally send
     1  "SPY"  to return to the  "SPY:"  prompt'  /
      DATA  TEXSPY ( 862 )   /  '            (from which  "GO"  will be 
     1required to actually make the plot  ROLL,'  /
      DATA  TEXSPY ( 863 )   /  '            if  "BREAK"  or  "LOCK"  we
     1re in effect at that point).   Should the'  /
      DATA  TEXSPY ( 864 )   /  '            user wish to cancel a rolli
     1ng vector plot from SPY, send  "NOROLL"  '  /
      DATA  TEXSPY ( 865 )   /  '            (which will stop both  ROLL
     1ing  vector and character plots).        '  /
      DATA  TEXSPY ( 866 )   /  '  ROLLC  -- To toggle the switch which 
     1produces a  ROLLing  character plot.  See'  /
      DATA  TEXSPY ( 867 )   /  '            preceding  "ROLLV"  for det
     1ails (functioning is comparable).        '  /
      DATA  TEXSPY ( 868 )   /  '               < < <  =======   END OF 
     1INNER-LEVEL  COMMANDS   =======  > > >   '  /
      DATA  KBEGTX (  1 )   /     1   /
      DATA  KBEGTX (  2 )   /     4   /
      DATA  KBEGTX (  3 )   /     9   /
      DATA  KBEGTX (  4 )   /    22   /
      DATA  KBEGTX (  5 )   /    29   /
      DATA  KBEGTX (  6 )   /    41   /
      DATA  KBEGTX (  7 )   /    53   /
      DATA  KBEGTX (  8 )   /    64   /
      DATA  KBEGTX (  9 )   /    69   /
      DATA  KBEGTX ( 10 )   /    79   /
      DATA  KBEGTX ( 11 )   /    88   /
      DATA  KBEGTX ( 12 )   /    97   /
      DATA  KBEGTX ( 13 )   /   112   /
      DATA  KBEGTX ( 14 )   /   119   /
      DATA  KBEGTX ( 15 )   /   136   /
      DATA  KBEGTX ( 16 )   /   141   /
      DATA  KBEGTX ( 17 )   /   154   /
      DATA  KBEGTX ( 18 )   /   158   /
      DATA  KBEGTX ( 19 )   /   162   /
      DATA  KBEGTX ( 20 )   /   202   /
      DATA  KBEGTX ( 21 )   /   217   /
      DATA  KBEGTX ( 22 )   /   228   /
      DATA  KBEGTX ( 23 )   /   237   /
      DATA  KBEGTX ( 24 )   /   242   /
      DATA  KBEGTX ( 25 )   /   252   /
      DATA  KBEGTX ( 26 )   /   263   /
      DATA  KBEGTX ( 27 )   /   294   /
      DATA  KBEGTX ( 28 )   /   303   /
      DATA  KBEGTX ( 29 )   /   317   /
      DATA  KBEGTX ( 30 )   /   325   /
      DATA  KBEGTX ( 31 )   /   333   /
      DATA  KBEGTX ( 32 )   /   340   /
      DATA  KBEGTX ( 33 )   /   345   /
      DATA  KBEGTX ( 34 )   /   361   /
      DATA  KBEGTX ( 35 )   /   377   /
      DATA  KBEGTX ( 36 )   /   381   /
      DATA  KBEGTX ( 37 )   /   425   /
      DATA  KBEGTX ( 38 )   /   436   /
      DATA  KBEGTX ( 39 )   /   446   /
      DATA  KBEGTX ( 40 )   /   449   /
      DATA  KBEGTX ( 41 )   /   453   /
      DATA  KBEGTX ( 42 )   /   456   /
      DATA  KBEGTX ( 43 )   /   467   /
      DATA  KBEGTX ( 44 )   /   471   /
      DATA  KBEGTX ( 45 )   /   482   /
      DATA  KBEGTX ( 46 )   /   486   /
      DATA  KBEGTX ( 47 )   /   490   /
      DATA  KBEGTX ( 48 )   /   497   /
      DATA  KBEGTX ( 49 )   /   503   /
      DATA  KBEGTX ( 50 )   /   514   /
      DATA  KBEGTX ( 51 )   /   559   /
      DATA  KBEGTX ( 52 )   /   586   /
      DATA  KBEGTX ( 53 )   /   621   /
      DATA  KBEGTX ( 54 )   /   630   /
      DATA  KBEGTX ( 55 )   /   641   /
      DATA  KBEGTX ( 56 )   /   647   /
      DATA  KBEGTX ( 57 )   /   657   /
      DATA  KBEGTX ( 58 )   /   661   /
      DATA  KBEGTX ( 59 )   /   665   /
      DATA  KBEGTX ( 60 )   /   679   /
      DATA  KBEGTX ( 61 )   /   683   /
      DATA  KBEGTX ( 62 )   /   691   /
      DATA  KBEGTX ( 63 )   /   720   /
      DATA  KBEGTX ( 64 )   /   725   /
      DATA  KBEGTX ( 65 )   /   752   /
      DATA  KBEGTX ( 66 )   /   766   /
      DATA  KBEGTX ( 67 )   /   777   /
      DATA  KBEGTX ( 68 )   /   812   /
      DATA  KBEGTX ( 69 )   /   869   /
      END
