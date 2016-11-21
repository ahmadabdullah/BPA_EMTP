      IMPLICIT  REAL*8 (A-H, O-Z),  INTEGER*4 (I-N)                             
      REAL*8   BUS1(1), TEXTA(2), TEXTB(2)                                      
      CHARACTER*16  ANSI16                                                      
      DIMENSION  CBLOCK(300),  NCBARR(300),  CBLSER(300),  JBLTYP(300)          
      DIMENSION  LSTNEW(99),  LSTDEF(49),  TYPE(4,3),  KEXTRA(29)               
      DIMENSION  ABUFF(14), CHAR(6), MULVAR(4)                                  
      DATA  CBLOCK(  1)  / 6HX      /,          NCBARR(  1)  /  3 /             
        DATA  CBLSER(  1)  / 6HC0B001 /,          JBLTYP(  1)  / 0 /            
      DATA  CBLOCK(  2)  / 6HYKM    /,          NCBARR(  2)  /  5 /             
        DATA  CBLSER(  2)  / 6HC0B002 /,          JBLTYP(  2)  / 0 /            
      DATA  CBLOCK(  3)  / 6HKM     /,          NCBARR(  3)  /  5 /             
        DATA  CBLSER(  3)  / 6HC0B003 /,          JBLTYP(  3)  / 0 /            
      DATA  CBLOCK(  4)  / 6HXK     /,          NCBARR(  4)  / 72 /             
        DATA  CBLSER(  4)  / 6HC0B004 /,          JBLTYP(  4)  / 0 /            
      DATA  CBLOCK(  5)  / 6HXM     /,          NCBARR(  5)  / 72 /             
        DATA  CBLSER(  5)  / 6HC0B005 /,          JBLTYP(  5)  / 0 /            
      DATA  CBLOCK(  6)  / 6HWEIGHT /,          NCBARR(  6)  / 14 /             
        DATA  CBLSER(  6)  / 6HC0B006 /,          JBLTYP(  6)  / 0 /            
      DATA  CBLOCK(  7)  / 6HIWTENT /,          NCBARR(  7)  / 52 /             
        DATA  CBLSER(  7)  / 6HC0B007 /,          JBLTYP(  7)  / 0 /            
      DATA  CBLOCK(  8)  / 6HCON1   /,          NCBARR(  8)  / 51 /             
        DATA  CBLSER(  8)  / 6HC0B008 /,          JBLTYP(  8)  / 0 /            
      DATA  CBLOCK(  9)  / 6HISKIP  /,          NCBARR(  9)  / 13 /             
        DATA  CBLSER(  9)  / 6HC0B009 /,          JBLTYP(  9)  / 0 /            
      DATA  CBLOCK( 10)  / 6HZINF   /,          NCBARR( 10)  / 13 /             
        DATA  CBLSER( 10)  / 6HC0B010 /,          JBLTYP( 10)  / 0 /            
      DATA  CBLOCK( 11)  / 6HETA    /,          NCBARR( 11)  / 13 /             
        DATA  CBLSER( 11)  / 6HC0B011 /,          JBLTYP( 11)  / 0 /            
      DATA  CBLOCK( 12)  / 6HNHIST  /,          NCBARR( 12)  / 13 /             
        DATA  CBLSER( 12)  / 6HC0B012 /,          JBLTYP( 12)  / 0 /            
      DATA  CBLOCK( 13)  / 6HSTAILM /,          NCBARR( 13)  / 15 /             
        DATA  CBLSER( 13)  / 6HC0B013 /,          JBLTYP( 13)  / 0 /            
      DATA  CBLOCK( 14)  / 6HSTAILK /,          NCBARR( 14)  / 15 /             
        DATA  CBLSER( 14)  / 6HC0B014 /,          JBLTYP( 14)  / 0 /            
      DATA  CBLOCK( 15)  / 6HXMAX   /,          NCBARR( 15)  / 58 /             
        DATA  CBLSER( 15)  / 6HC0B015 /,          JBLTYP( 15)  / 0 /            
      DATA  CBLOCK( 16)  / 6HKOUTVP /,          NCBARR( 16)  / 62 /             
        DATA  CBLSER( 16)  / 6HC0B016 /,          JBLTYP( 16)  / 0 /            
      DATA  CBLOCK( 17)  / 6HBNRG   /,          NCBARR( 17)  / 18 /             
        DATA  CBLSER( 17)  / 6HC0B017 /,          JBLTYP( 17)  / 0 /            
      DATA  CBLOCK( 18)  / 6HSCONST /,          NCBARR( 18)  / 20 /             
        DATA  CBLSER( 18)  / 6HC0B018 /,          JBLTYP( 18)  / 0 /            
      DATA  CBLOCK( 19)  / 6HCNVHST /,          NCBARR( 19)  / 73 /             
        DATA  CBLSER( 19)  / 6HC0B019 /,          JBLTYP( 19)  / 0 /            
      DATA  CBLOCK( 20)  / 6HSFD    /,          NCBARR( 20)  / 71 /             
        DATA  CBLSER( 20)  / 6HC0B020 /,          JBLTYP( 20)  / 0 /            
      DATA  CBLOCK( 21)  / 6HQFD    /,          NCBARR( 21)  / 71 /             
        DATA  CBLSER( 21)  / 6HC0B021 /,          JBLTYP( 21)  / 0 /            
      DATA  CBLOCK( 22)  / 6HSEMAUX /,          NCBARR( 22)  / 22 /             
        DATA  CBLSER( 22)  / 6HC0B022 /,          JBLTYP( 22)  / 0 /            
      DATA  CBLOCK( 23)  / 6HIBSOUT /,          NCBARR( 23)  / 12 /             
        DATA  CBLSER( 23)  / 6HC0B023 /,          JBLTYP( 23)  / 0 /            
      DATA  CBLOCK( 24)  / 6HBVALUE /,          NCBARR( 24)  / 12 /             
        DATA  CBLSER( 24)  / 6HC0B024 /,          JBLTYP( 24)  / 0 /            
      DATA  CBLOCK( 25)  / 6HSPTACS /,          NCBARR( 25)  / 19 /             
        DATA  CBLSER( 25)  / 6HC0B025 /,          JBLTYP( 25)  / 0 /            
      DATA  CBLOCK( 26)  / 6HKSWTYP /,          NCBARR( 26)  /  6 /             
        DATA  CBLSER( 26)  / 6HC0B026 /,          JBLTYP( 26)  / 0 /            
      DATA  CBLOCK( 27)  / 6HMODSWT /,          NCBARR( 27)  /  6 /             
        DATA  CBLSER( 27)  / 6HC0B027 /,          JBLTYP( 27)  / 0 /            
      DATA  CBLOCK( 28)  / 6HKBEGSW /,          NCBARR( 28)  /  6 /             
        DATA  CBLSER( 28)  / 6HC0B028 /,          JBLTYP( 28)  / 0 /            
      DATA  CBLOCK( 29)  / 6HLASTSW /,          NCBARR( 29)  /  6 /             
        DATA  CBLSER( 29)  / 6HC0B029 /,          JBLTYP( 29)  / 0 /            
      DATA  CBLOCK( 30)  / 6HKENTNB /,          NCBARR( 30)  /  6 /             
        DATA  CBLSER( 30)  / 6HC0B030 /,          JBLTYP( 30)  / 0 /            
      DATA  CBLOCK( 31)  / 6HNBHDSW /,          NCBARR( 31)  / 63 /             
        DATA  CBLSER( 31)  / 6HC0B031 /,          JBLTYP( 31)  / 0 /            
      DATA  CBLOCK( 32)  / 6HTOPEN  /,          NCBARR( 32)  / 60 /             
        DATA  CBLSER( 32)  / 6HC0B032 /,          JBLTYP( 32)  / 0 /            
      DATA  CBLOCK( 33)  / 6HCRIT   /,          NCBARR( 33)  /  6 /             
        DATA  CBLSER( 33)  / 6HC0B033 /,          JBLTYP( 33)  / 0 /            
      DATA  CBLOCK( 34)  / 6HKDEPSW /,          NCBARR( 34)  / 60 /             
        DATA  CBLSER( 34)  / 6HC0B034 /,          JBLTYP( 34)  / 0 /            
      DATA  CBLOCK( 35)  / 6HTDNS   /,          NCBARR( 35)  /  6 /             
        DATA  CBLSER( 35)  / 6HC0B035 /,          JBLTYP( 35)  / 0 /            
      DATA  CBLOCK( 36)  / 6HISOURC /,          NCBARR( 36)  /  6 /             
        DATA  CBLSER( 36)  / 6HC0B036 /,          JBLTYP( 36)  / 0 /            
      DATA  CBLOCK( 37)  / 6HENERGY /,          NCBARR( 37)  /  6 /             
        DATA  CBLSER( 37)  / 6HC0B037 /,          JBLTYP( 37)  / 0 /            
      DATA  CBLOCK( 38)  / 6HIARDUB /,          NCBARR( 38)  / 63 /             
        DATA  CBLSER( 38)  / 6HC0B038 /,          JBLTYP( 38)  / 0 /            
      DATA  CBLOCK( 39)  / 6HARDUBE /,          NCBARR( 39)  / 64 /             
        DATA  CBLSER( 39)  / 6HC0B039 /,          JBLTYP( 39)  / 0 /            
      DATA  CBLOCK( 40)  / 6HNONLAD /,          NCBARR( 40)  /  9 /             
        DATA  CBLSER( 40)  / 6HC0B040 /,          JBLTYP( 40)  / 0 /            
      DATA  CBLOCK( 41)  / 6HNONLE  /,          NCBARR( 41)  /  9 /             
        DATA  CBLSER( 41)  / 6HC0B041 /,          JBLTYP( 41)  / 0 /            
      DATA  CBLOCK( 42)  / 6HVNONL  /,          NCBARR( 42)  /  9 /             
        DATA  CBLSER( 42)  / 6HC0B042 /,          JBLTYP( 42)  / 0 /            
      DATA  CBLOCK( 43)  / 6HCURR   /,          NCBARR( 43)  /  9 /             
        DATA  CBLSER( 43)  / 6HC0B043 /,          JBLTYP( 43)  / 0 /            
      DATA  CBLOCK( 44)  / 6HANONL  /,          NCBARR( 44)  /  9 /             
        DATA  CBLSER( 44)  / 6HC0B044 /,          JBLTYP( 44)  / 0 /            
      DATA  CBLOCK( 45)  / 6HVECNL1 /,          NCBARR( 45)  /  9 /             
        DATA  CBLSER( 45)  / 6HC0B045 /,          JBLTYP( 45)  / 0 /            
      DATA  CBLOCK( 46)  / 6HVECNL2 /,          NCBARR( 46)  /  9 /             
        DATA  CBLSER( 46)  / 6HC0B046 /,          JBLTYP( 46)  / 0 /            
      DATA  CBLOCK( 47)  / 6HNAMENL /,          NCBARR( 47)  /  9 /             
        DATA  CBLSER( 47)  / 6HC0B047 /,          JBLTYP( 47)  / 0 /            
      DATA  CBLOCK( 48)  / 6HVZERO  /,          NCBARR( 48)  /  9 /             
        DATA  CBLSER( 48)  / 6HC0B048 /,          JBLTYP( 48)  / 0 /            
      DATA  CBLOCK( 49)  / 6HILAST  /,          NCBARR( 49)  /  9 /             
        DATA  CBLSER( 49)  / 6HC0B049 /,          JBLTYP( 49)  / 0 /            
      DATA  CBLOCK( 50)  / 6HNLTYPE /,          NCBARR( 50)  /  9 /             
        DATA  CBLSER( 50)  / 6HC0B050 /,          JBLTYP( 50)  / 0 /            
      DATA  CBLOCK( 51)  / 6HKUPL   /,          NCBARR( 51)  /  9 /             
        DATA  CBLSER( 51)  / 6HC0B051 /,          JBLTYP( 51)  / 0 /            
      DATA  CBLOCK( 52)  / 6HNLSUB  /,          NCBARR( 52)  /  9 /             
        DATA  CBLSER( 52)  / 6HC0B052 /,          JBLTYP( 52)  / 0 /            
      DATA  CBLOCK( 53)  / 6HXOPTBR /,          NCBARR( 53)  /  2 /             
        DATA  CBLSER( 53)  / 6HC0B053 /,          JBLTYP( 53)  / 0 /            
      DATA  CBLOCK( 54)  / 6HCOPTBR /,          NCBARR( 54)  /  2 /             
        DATA  CBLSER( 54)  / 6HC0B054 /,          JBLTYP( 54)  / 0 /            
      DATA  CBLOCK( 55)  / 6HCURSUB /,          NCBARR( 55)  / 53 /             
        DATA  CBLSER( 55)  / 6HC0B055 /,          JBLTYP( 55)  / 0 /            
      DATA  CBLOCK( 56)  / 6HCCHAR  /,          NCBARR( 56)  / 10 /             
        DATA  CBLSER( 56)  / 6HC0B056 /,          JBLTYP( 56)  / 0 /            
      DATA  CBLOCK( 57)  / 6HVCHAR  /,          NCBARR( 57)  / 10 /             
        DATA  CBLSER( 57)  / 6HC0B057 /,          JBLTYP( 57)  / 0 /            
      DATA  CBLOCK( 58)  / 6HGSLOPE /,          NCBARR( 58)  / 10 /             
        DATA  CBLSER( 58)  / 6HC0B058 /,          JBLTYP( 58)  / 0 /            
      DATA  CBLOCK( 59)  / 6HKTRANS /,          NCBARR( 59)  /  1 /             
        DATA  CBLSER( 59)  / 6HC0B059 /,          JBLTYP( 59)  / 0 /            
      DATA  CBLOCK( 60)  / 6HKK     /,          NCBARR( 60)  /  1 /             
        DATA  CBLSER( 60)  / 6HC0B060 /,          JBLTYP( 60)  / 0 /            
      DATA  CBLOCK( 61)  / 6HC      /,          NCBARR( 61)  /  3 /             
        DATA  CBLSER( 61)  / 6HC0B061 /,          JBLTYP( 61)  / 0 /            
      DATA  CBLOCK( 62)  / 6HTR     /,          NCBARR( 62)  /  5 /             
        DATA  CBLSER( 62)  / 6HC0B062 /,          JBLTYP( 62)  / 0 /            
      DATA  CBLOCK( 63)  / 6HTX     /,          NCBARR( 63)  /  5 /             
        DATA  CBLSER( 63)  / 6HC0B063 /,          JBLTYP( 63)  / 0 /            
      DATA  CBLOCK( 64)  / 6HR      /,          NCBARR( 64)  /  3 /             
        DATA  CBLSER( 64)  / 6HC0B064 /,          JBLTYP( 64)  / 0 /            
      DATA  CBLOCK( 65)  / 6HNR     /,          NCBARR( 65)  /  2 /             
        DATA  CBLSER( 65)  / 6HC0B065 /,          JBLTYP( 65)  / 0 /            
      DATA  CBLOCK( 66)  / 6HLENGTH /,          NCBARR( 66)  /  2 /             
        DATA  CBLSER( 66)  / 6HC0B066 /,          JBLTYP( 66)  / 0 /            
      DATA  CBLOCK( 67)  / 6HCIK    /,          NCBARR( 67)  /  2 /             
        DATA  CBLSER( 67)  / 6HC0B067 /,          JBLTYP( 67)  / 0 /            
      DATA  CBLOCK( 68)  / 6HCI     /,          NCBARR( 68)  /  2 /             
        DATA  CBLSER( 68)  / 6HC0B068 /,          JBLTYP( 68)  / 0 /            
      DATA  CBLOCK( 69)  / 6HCK     /,          NCBARR( 69)  /  2 /             
        DATA  CBLSER( 69)  / 6HC0B069 /,          JBLTYP( 69)  / 0 /            
      DATA  CBLOCK( 70)  / 6HISMOUT /,          NCBARR( 70)  / 70 /             
        DATA  CBLSER( 70)  / 6HC0B070 /,          JBLTYP( 70)  / 0 /            
      DATA  CBLOCK( 71)  / 6HELP    /,          NCBARR( 71)  / 65 /             
        DATA  CBLSER( 71)  / 6HC0B071 /,          JBLTYP( 71)  / 0 /            
      DATA  CBLOCK( 72)  / 6HCU     /,          NCBARR( 72)  / 66 /             
        DATA  CBLSER( 72)  / 6HC0B072 /,          JBLTYP( 72)  / 0 /            
      DATA  CBLOCK( 73)  / 6HSHP    /,          NCBARR( 73)  / 67 /             
        DATA  CBLSER( 73)  / 6HC0B073 /,          JBLTYP( 73)  / 0 /            
      DATA  CBLOCK( 74)  / 6HHISTQ  /,          NCBARR( 74)  / 68 /             
        DATA  CBLSER( 74)  / 6HC0B074 /,          JBLTYP( 74)  / 0 /            
      DATA  CBLOCK( 75)  / 6HISMDAT /,          NCBARR( 75)  / 69 /             
        DATA  CBLSER( 75)  / 6HC0B075 /,          JBLTYP( 75)  / 0 /            
      DATA  CBLOCK( 76)  / 6HTEXVEC /,          NCBARR( 76)  /  7 /             
        DATA  CBLSER( 76)  / 6HC0B076 /,          JBLTYP( 76)  / 1 /            
      DATA  CBLOCK( 77)  / 6HIBRNCH /,          NCBARR( 77)  / 12 /             
        DATA  CBLSER( 77)  / 6HC0B077 /,          JBLTYP( 77)  / 0 /            
      DATA  CBLOCK( 78)  / 6HJBRNCH /,          NCBARR( 78)  / 12 /             
        DATA  CBLSER( 78)  / 6HC0B078 /,          JBLTYP( 78)  / 0 /            
      DATA  CBLOCK( 79)  / 6HTSTOP  /,          NCBARR( 79)  /  4 /             
        DATA  CBLSER( 79)  / 6HC0B079 /,          JBLTYP( 79)  / 0 /            
      DATA  CBLOCK( 80)  / 6HNONLK  /,          NCBARR( 80)  /  9 /             
        DATA  CBLSER( 80)  / 6HC0B080 /,          JBLTYP( 80)  / 0 /            
      DATA  CBLOCK( 81)  / 6HNONLM  /,          NCBARR( 81)  /  9 /             
        DATA  CBLSER( 81)  / 6HC0B081 /,          JBLTYP( 81)  / 0 /            
      DATA  CBLOCK( 82)  / 6HSPUM   /,          NCBARR( 82)  / 25 /             
        DATA  CBLSER( 82)  / 6HC0B082 /,          JBLTYP( 82)  / 0 /            
      DATA  CBLOCK( 83)  / 6HKKS    /,          NCBARR( 83)  /  1 /             
        DATA  CBLSER( 83)  / 6HC0B083 /,          JBLTYP( 83)  / 0 /            
      DATA  CBLOCK( 84)  / 6HKKNONL /,          NCBARR( 84)  / 57 /             
        DATA  CBLSER( 84)  / 6HC0B084 /,          JBLTYP( 84)  / 0 /            
      DATA  CBLOCK( 85)  / 6HZNONL  /,          NCBARR( 85)  / 57 /             
        DATA  CBLSER( 85)  / 6HC0B085 /,          JBLTYP( 85)  / 0 /            
      DATA  CBLOCK( 86)  / 6HZNONLB /,          NCBARR( 86)  /  1 /             
        DATA  CBLSER( 86)  / 6HC0B086 /,          JBLTYP( 86)  / 0 /            
      DATA  CBLOCK( 87)  / 6HZNONLC /,          NCBARR( 87)  /  1 /             
        DATA  CBLSER( 87)  / 6HC0B087 /,          JBLTYP( 87)  / 0 /            
      DATA  CBLOCK( 88)  / 6HFINIT  /,          NCBARR( 88)  /  1 /             
        DATA  CBLSER( 88)  / 6HC0B088 /,          JBLTYP( 88)  / 0 /            
      DATA  CBLOCK( 89)  / 6HKSUB   /,          NCBARR( 89)  / 53 /             
        DATA  CBLSER( 89)  / 6HC0B089 /,          JBLTYP( 89)  / 0 /            
      DATA  CBLOCK( 90)  / 6HMSUB   /,          NCBARR( 90)  / 53 /             
        DATA  CBLSER( 90)  / 6HC0B090 /,          JBLTYP( 90)  / 0 /            
      DATA  CBLOCK( 91)  / 6HISUBEG /,          NCBARR( 91)  / 55 /             
        DATA  CBLSER( 91)  / 6HC0B091 /,          JBLTYP( 91)  / 0 /            
      DATA  CBLOCK( 92)  / 6HLITYPE /,          NCBARR( 92)  /  2 /             
        DATA  CBLSER( 92)  / 6HC0B092 /,          JBLTYP( 92)  / 0 /            
      DATA  CBLOCK( 93)  / 6HIMODEL /,          NCBARR( 93)  /  2 /             
        DATA  CBLSER( 93)  / 6HC0B093 /,          JBLTYP( 93)  / 0 /            
      DATA  CBLOCK( 94)  / 6HKBUS   /,          NCBARR( 94)  /  2 /             
        DATA  CBLSER( 94)  / 6HC0B094 /,          JBLTYP( 94)  / 0 /            
      DATA  CBLOCK( 95)  / 6HMBUS   /,          NCBARR( 95)  /  2 /             
        DATA  CBLSER( 95)  / 6HC0B095 /,          JBLTYP( 95)  / 0 /            
      DATA  CBLOCK( 96)  / 6HKODEBR /,          NCBARR( 96)  /  2 /             
        DATA  CBLSER( 96)  / 6HC0B096 /,          JBLTYP( 96)  / 0 /            
      DATA  CBLOCK( 97)  / 6HCKI    /,          NCBARR( 97)  /  2 /             
        DATA  CBLSER( 97)  / 6HC0B097 /,          JBLTYP( 97)  / 0 /            
      DATA  CBLOCK( 98)  / 6HCKKJM  /,          NCBARR( 98)  /  2 /             
        DATA  CBLSER( 98)  / 6HC0B098 /,          JBLTYP( 98)  / 0 /            
      DATA  CBLOCK( 99)  / 6HINDHST /,          NCBARR( 99)  /  2 /             
        DATA  CBLSER( 99)  / 6HC0B099 /,          JBLTYP( 99)  / 0 /            
      DATA  CBLOCK(100)  / 6HKODSEM /,          NCBARR(100)  /  2 /             
        DATA  CBLSER(100)  / 6HC0B100 /,          JBLTYP(100)  / 0 /            
      DATA  CBLOCK(101)  / 6HNAMEBR /,          NCBARR(101)  / 54 /             
        DATA  CBLSER(101)  / 6HC0B101 /,          JBLTYP(101)  / 0 /            
      DATA  CBLOCK(102)  / 6HIFORM  /,          NCBARR(102)  /  4 /             
        DATA  CBLSER(102)  / 6HC0B102 /,          JBLTYP(102)  / 0 /            
      DATA  CBLOCK(103)  / 6HNODE   /,          NCBARR(103)  /  4 /             
        DATA  CBLSER(103)  / 6HC0B103 /,          JBLTYP(103)  / 0 /            
      DATA  CBLOCK(104)  / 6HCREST  /,          NCBARR(104)  /  4 /             
        DATA  CBLSER(104)  / 6HC0B104 /,          JBLTYP(104)  / 0 /            
      DATA  CBLOCK(105)  / 6HTIME1  /,          NCBARR(105)  /  4 /             
        DATA  CBLSER(105)  / 6HC0B105 /,          JBLTYP(105)  / 0 /            
      DATA  CBLOCK(106)  / 6HTIME2  /,          NCBARR(106)  /  4 /             
        DATA  CBLSER(106)  / 6HC0B106 /,          JBLTYP(106)  / 0 /            
      DATA  CBLOCK(107)  / 6HTSTART /,          NCBARR(107)  /  4 /             
        DATA  CBLSER(107)  / 6HC0B107 /,          JBLTYP(107)  / 0 /            
      DATA  CBLOCK(108)  / 6HSFREQ  /,          NCBARR(108)  /  4 /             
        DATA  CBLSER(108)  / 6HC0B108 /,          JBLTYP(108)  / 0 /            
      DATA  CBLOCK(109)  / 6HKMSWIT /,          NCBARR(109)  / 60 /             
        DATA  CBLSER(109)  / 6HC0B109 /,          JBLTYP(109)  / 0 /            
      DATA  CBLOCK(110)  / 6HNEXTSW /,          NCBARR(110)  /  6 /             
        DATA  CBLSER(110)  / 6HC0B110 /,          JBLTYP(110)  / 0 /            
      DATA  CBLOCK(111)  / 6HRMFD   /,          NCBARR(111)  / 61 /             
        DATA  CBLSER(111)  / 6HC0B111 /,          JBLTYP(111)  / 0 /            
      DATA  CBLOCK(112)  / 6HCIKFD  /,          NCBARR(112)  / 61 /             
        DATA  CBLSER(112)  / 6HC0B112 /,          JBLTYP(112)  / 0 /            
      DATA  CBLOCK(113)  / 6HIMFD   /,          NCBARR(113)  / 27 /             
        DATA  CBLSER(113)  / 6HC0B113 /,          JBLTYP(113)  / 0 /            
      DATA  CBLOCK(114)  / 6HTCLOSE /,          NCBARR(114)  /  6 /             
        DATA  CBLSER(114)  / 6HC0B114 /,          JBLTYP(114)  / 0 /            
      DATA  CBLOCK(115)  / 6HADELAY /,          NCBARR(115)  / 60 /             
        DATA  CBLSER(115)  / 6HC0B115 /,          JBLTYP(115)  / 0 /            
      DATA  CBLOCK(116)  / 6HKPOS   /,          NCBARR(116)  /  6 /             
        DATA  CBLSER(116)  / 6HC0B116 /,          JBLTYP(116)  / 0 /            
      DATA  CBLOCK(117)  / 6HNAMESW /,          NCBARR(117)  /  6 /             
        DATA  CBLSER(117)  / 6HC0B117 /,          JBLTYP(117)  / 0 /            
      DATA  CBLOCK(118)  / 6HE      /,          NCBARR(118)  /  1 /             
        DATA  CBLSER(118)  / 6HC0B118 /,          JBLTYP(118)  / 0 /            
      DATA  CBLOCK(119)  / 6HF      /,          NCBARR(119)  /  1 /             
        DATA  CBLSER(119)  / 6HC0B119 /,          JBLTYP(119)  / 0 /            
      DATA  CBLOCK(120)  / 6HKSSFRQ /,          NCBARR(120)  /  1 /             
        DATA  CBLSER(120)  / 6HC0B120 /,          JBLTYP(120)  / 0 /            
      DATA  CBLOCK(121)  / 6HKODE   /,          NCBARR(121)  /  1 /             
        DATA  CBLSER(121)  / 6HC0B121 /,          JBLTYP(121)  / 0 /            
      DATA  CBLOCK(122)  / 6HKPSOUR /,          NCBARR(122)  /  1 /             
        DATA  CBLSER(122)  / 6HC0B122 /,          JBLTYP(122)  / 0 /            
      DATA  CBLOCK(123)  / 6HVOLTI  /,          NCBARR(123)  / 59 /             
        DATA  CBLSER(123)  / 6HC0B123 /,          JBLTYP(123)  / 0 /            
      DATA  CBLOCK(124)  / 6HVOLTK  /,          NCBARR(124)  / 26 /             
        DATA  CBLSER(124)  / 6HC0B124 /,          JBLTYP(124)  / 0 /            
      DATA  CBLOCK(125)  / 6HVOLT   /,          NCBARR(125)  / 59 /             
        DATA  CBLSER(125)  / 6HC0B125 /,          JBLTYP(125)  / 0 /            
      DATA  CBLOCK(126)  / 6HBUS    /,          NCBARR(126)  /  1 /             
        DATA  CBLSER(126)  / 6HC0B126 /,          JBLTYP(126)  / 1 /            
      DATA  CBLOCK(127)  / 6HKARRAY /,          NCBARR(127)  /  0 /             
        DATA  CBLSER(127)  / 6HC29B01 /,          JBLTYP(127)  / 0 /            
      DATA  CBLOCK(128)  / 6HTP     /,          NCBARR(128)  / 23 /             
        DATA  CBLSER(128)  / 6HSPAC01 /,          JBLTYP(128)  / 0 /            
      DATA  CBLOCK(129)  / 6HNORDER /,          NCBARR(129)  /  1 /             
        DATA  CBLSER(129)  / 6HSPAC02 /,          JBLTYP(129)  / 0 /            
      DATA  CBLOCK(130)  / 6HINDEX  /,          NCBARR(130)  /  1 /             
        DATA  CBLSER(130)  / 6HSPAC03 /,          JBLTYP(130)  / 0 /            
      DATA  CBLOCK(131)  / 6HDIAG   /,          NCBARR(131)  /  1 /             
        DATA  CBLSER(131)  / 6HSPAC04 /,          JBLTYP(131)  / 0 /            
      DATA  CBLOCK(132)  / 6HDIAB   /,          NCBARR(132)  /  1 /             
        DATA  CBLSER(132)  / 6HSPAC05 /,          JBLTYP(132)  / 0 /            
      DATA  CBLOCK(133)  / 6HSOLR   /,          NCBARR(133)  /  1 /             
        DATA  CBLSER(133)  / 6HSPAC06 /,          JBLTYP(133)  / 0 /            
      DATA  CBLOCK(134)  / 6HSOLI   /,          NCBARR(134)  /  1 /             
        DATA  CBLSER(134)  / 6HSPAC07 /,          JBLTYP(134)  / 0 /            
      DATA  CBLOCK(135)  / 6HICH1   /,          NCBARR(135)  /  1 /             
        DATA  CBLSER(135)  / 6HSPAC08 /,          JBLTYP(135)  / 0 /            
      DATA  CBLOCK(136)  / 6HBND    /,          NCBARR(136)  /  9 /             
        DATA  CBLSER(136)  / 6HSPAC09 /,          JBLTYP(136)  / 0 /            
      DATA  CBLOCK(137)  / 6HILOC   /,          NCBARR(137)  / 23 /             
        DATA  CBLSER(137)  / 6HSPAC10 /,          JBLTYP(137)  / 0 /            
      DATA  CBLOCK(138)  / 6HGND    /,          NCBARR(138)  / 23 /             
        DATA  CBLSER(138)  / 6HSPAC11 /,          JBLTYP(138)  / 0 /            
      DATA  CBLOCK(139)  / 6HKARRAY /,          NCBARR(139)  /  9 /             
        DATA  CBLSER(139)  / 6HC31B01 /,          JBLTYP(139)  / 0 /            
      DATA  CBLOCK(140)  / 6HXDAT   /,          NCBARR(140)  / 71 /             
        DATA  CBLSER(140)  / 6HC39B01 /,          JBLTYP(140)  / 0 /            
      DATA  CBLOCK(141)  / 6HYDAT   /,          NCBARR(141)  / 71 /             
        DATA  CBLSER(141)  / 6HC39B02 /,          JBLTYP(141)  / 0 /            
      DATA  CBLOCK(142)  / 6HAPHDAT /,          NCBARR(142)  / 71 /             
        DATA  CBLSER(142)  / 6HC39B03 /,          JBLTYP(142)  / 0 /            
      DATA  CBLOCK(143)  / 6HJNDEX  /,          NCBARR(143)  /  1 /             
        DATA  CBLSER(143)  / 6HC10B01 /,          JBLTYP(143)  / 0 /            
      DATA  CBLOCK(144)  / 6HDIAGG  /,          NCBARR(144)  /  1 /             
        DATA  CBLSER(144)  / 6HC10B02 /,          JBLTYP(144)  / 0 /            
      DATA  CBLOCK(145)  / 6HDIABB  /,          NCBARR(145)  /  1 /             
        DATA  CBLSER(145)  / 6HC10B03 /,          JBLTYP(145)  / 0 /            
      DATA  CBLOCK(146)  / 6HSOLRSV /,          NCBARR(146)  /  1 /             
        DATA  CBLSER(146)  / 6HC10B04 /,          JBLTYP(146)  / 0 /            
      DATA  CBLOCK(147)  / 6HSOLISV /,          NCBARR(147)  /  1 /             
        DATA  CBLSER(147)  / 6HC10B05 /,          JBLTYP(147)  / 0 /            
      DATA  CBLOCK(148)  / 6HGNDD   /,          NCBARR(148)  / 23 /             
        DATA  CBLSER(148)  / 6HC10B06 /,          JBLTYP(148)  / 0 /            
      DATA  CBLOCK(149)  / 6HBNDD   /,          NCBARR(149)  / 23 /             
        DATA  CBLSER(149)  / 6HC10B07 /,          JBLTYP(149)  / 0 /            
      DATA  CBLOCK(150)  / 6HNEKFIX /,          NCBARR(150)  /  4 /             
        DATA  CBLSER(150)  / 6HC10B08 /,          JBLTYP(150)  / 0 /            
      DATA  CBLOCK(151)  / 6HFXTEM1 /,          NCBARR(151)  /  4 /             
        DATA  CBLSER(151)  / 6HC10B09 /,          JBLTYP(151)  / 0 /            
      DATA  CBLOCK(152)  / 6HFXTEM2 /,          NCBARR(152)  /  4 /             
        DATA  CBLSER(152)  / 6HC10B10 /,          JBLTYP(152)  / 0 /            
      DATA  CBLOCK(153)  / 6HFXTEM3 /,          NCBARR(153)  /  4 /             
        DATA  CBLSER(153)  / 6HC10B11 /,          JBLTYP(153)  / 0 /            
      DATA  CBLOCK(154)  / 6HFXTEM4 /,          NCBARR(154)  /  4 /             
        DATA  CBLSER(154)  / 6HC10B12 /,          JBLTYP(154)  / 0 /            
      DATA  CBLOCK(155)  / 6HFXTEM5 /,          NCBARR(155)  /  4 /             
        DATA  CBLSER(155)  / 6HC10B13 /,          JBLTYP(155)  / 0 /            
      DATA  CBLOCK(156)  / 6HFXTEM6 /,          NCBARR(156)  /  4 /             
        DATA  CBLSER(156)  / 6HC10B14 /,          JBLTYP(156)  / 0 /            
      DATA  CBLOCK(157)  / 6HFIXBU1 /,          NCBARR(157)  /  4 /             
        DATA  CBLSER(157)  / 6HC10B15 /,          JBLTYP(157)  / 0 /            
      DATA  CBLOCK(158)  / 6HFIXBU2 /,          NCBARR(158)  /  4 /             
        DATA  CBLSER(158)  / 6HC10B16 /,          JBLTYP(158)  / 0 /            
      DATA  CBLOCK(159)  / 6HFIXBU3 /,          NCBARR(159)  /  4 /             
        DATA  CBLSER(159)  / 6HC10B17 /,          JBLTYP(159)  / 0 /            
      DATA  CBLOCK(160)  / 6HFIXBU4 /,          NCBARR(160)  /  4 /             
        DATA  CBLSER(160)  / 6HC10B18 /,          JBLTYP(160)  / 0 /            
      DATA  CBLOCK(161)  / 6HFIXBU5 /,          NCBARR(161)  /  4 /             
        DATA  CBLSER(161)  / 6HC10B19 /,          JBLTYP(161)  / 0 /            
      DATA  CBLOCK(162)  / 6HFIXBU6 /,          NCBARR(162)  /  4 /             
        DATA  CBLSER(162)  / 6HC10B20 /,          JBLTYP(162)  / 0 /            
      DATA  CBLOCK(163)  / 6HFIXBU7 /,          NCBARR(163)  /  4 /             
        DATA  CBLSER(163)  / 6HC10B21 /,          JBLTYP(163)  / 0 /            
      DATA  CBLOCK(164)  / 6HFIXBU8 /,          NCBARR(164)  /  4 /             
        DATA  CBLSER(164)  / 6HC10B22 /,          JBLTYP(164)  / 0 /            
      DATA  CBLOCK(165)  / 6HFIXBU9 /,          NCBARR(165)  /  4 /             
        DATA  CBLSER(165)  / 6HC10B23 /,          JBLTYP(165)  / 0 /            
      DATA  CBLOCK(166)  / 6HFIXB10 /,          NCBARR(166)  /  4 /             
        DATA  CBLSER(166)  / 6HC10B24 /,          JBLTYP(166)  / 0 /            
      DATA  CBLOCK(167)  / 6HFIXB11 /,          NCBARR(167)  /  4 /             
        DATA  CBLSER(167)  / 6HC10B25 /,          JBLTYP(167)  / 0 /            
      DATA  CBLOCK(168)  / 6HKNDEX  /,          NCBARR(168)  /  4 /             
        DATA  CBLSER(168)  / 6HC10B26 /,          JBLTYP(168)  / 0 /            
      DATA  CBLOCK(169)  / 6HKARRAY /,          NCBARR(169)  /  9 /             
        DATA  CBLSER(169)  / 6HC44B01 /,          JBLTYP(169)  / 0 /            
      DATA  CBLOCK(170)  / 6HP      /,          NCBARR(170)  / 75 /             
        DATA  CBLSER(170)  / 6HC44B02 /,          JBLTYP(170)  / 0 /            
      DATA  CBLOCK(171)  / 6HZ      /,          NCBARR(171)  / 75 /             
        DATA  CBLSER(171)  / 6HC44B03 /,          JBLTYP(171)  / 0 /            
      DATA  CBLOCK(172)  / 6HIC     /,          NCBARR(172)  / 71 /             
        DATA  CBLSER(172)  / 6HC44B04 /,          JBLTYP(172)  / 0 /            
      DATA  CBLOCK(173)  / 6HR      /,          NCBARR(173)  / 71 /             
        DATA  CBLSER(173)  / 6HC44B05 /,          JBLTYP(173)  / 0 /            
      DATA  CBLOCK(174)  / 6HD      /,          NCBARR(174)  / 71 /             
        DATA  CBLSER(174)  / 6HC44B06 /,          JBLTYP(174)  / 0 /            
      DATA  CBLOCK(175)  / 6HGMD    /,          NCBARR(175)  / 71 /             
        DATA  CBLSER(175)  / 6HC44B07 /,          JBLTYP(175)  / 0 /            
      DATA  CBLOCK(176)  / 6HX      /,          NCBARR(176)  / 71 /             
        DATA  CBLSER(176)  / 6HC44B08 /,          JBLTYP(176)  / 0 /            
      DATA  CBLOCK(177)  / 6HY      /,          NCBARR(177)  / 71 /             
        DATA  CBLSER(177)  / 6HC44B09 /,          JBLTYP(177)  / 0 /            
      DATA  CBLOCK(178)  / 6HTB2    /,          NCBARR(178)  / 71 /             
        DATA  CBLSER(178)  / 6HC44B10 /,          JBLTYP(178)  / 0 /            
      DATA  CBLOCK(179)  / 6HITB3   /,          NCBARR(179)  / 71 /             
        DATA  CBLSER(179)  / 6HC44B11 /,          JBLTYP(179)  / 0 /            
      DATA  CBLOCK(180)  / 6HWORKR1 /,          NCBARR(180)  / 71 /             
        DATA  CBLSER(180)  / 6HC44B12 /,          JBLTYP(180)  / 0 /            
      DATA  CBLOCK(181)  / 6HWORKR2 /,          NCBARR(181)  / 71 /             
        DATA  CBLSER(181)  / 6HC44B13 /,          JBLTYP(181)  / 0 /            
      DATA  CBLOCK(182)  / 6HTEXT   /,          NCBARR(182)  / 76 /             
        DATA  CBLSER(182)  / 6HC44B14 /,          JBLTYP(182)  / 1 /            
      DATA  CBLOCK(183)  / 6HGD     /,          NCBARR(183)  / 74 /             
        DATA  CBLSER(183)  / 6HC44B15 /,          JBLTYP(183)  / 0 /            
      DATA  CBLOCK(184)  / 6HBD     /,          NCBARR(184)  / 74 /             
        DATA  CBLSER(184)  / 6HC44B16 /,          JBLTYP(184)  / 0 /            
      DATA  CBLOCK(185)  / 6HYD     /,          NCBARR(185)  / 74 /             
        DATA  CBLSER(185)  / 6HC44B17 /,          JBLTYP(185)  / 0 /            
      DATA  CBLOCK(186)  / 6HITBIC  /,          NCBARR(186)  / 73 /             
        DATA  CBLSER(186)  / 6HC44B18 /,          JBLTYP(186)  / 0 /            
      DATA  CBLOCK(187)  / 6HTBR    /,          NCBARR(187)  / 73 /             
        DATA  CBLSER(187)  / 6HC44B19 /,          JBLTYP(187)  / 0 /            
      DATA  CBLOCK(188)  / 6HTBD    /,          NCBARR(188)  / 73 /             
        DATA  CBLSER(188)  / 6HC44B20 /,          JBLTYP(188)  / 0 /            
      DATA  CBLOCK(189)  / 6HTBG    /,          NCBARR(189)  / 73 /             
        DATA  CBLSER(189)  / 6HC44B21 /,          JBLTYP(189)  / 0 /            
      DATA  CBLOCK(190)  / 6HTBX    /,          NCBARR(190)  / 73 /             
        DATA  CBLSER(190)  / 6HC44B22 /,          JBLTYP(190)  / 0 /            
      DATA  CBLOCK(191)  / 6HTBY    /,          NCBARR(191)  / 73 /             
        DATA  CBLSER(191)  / 6HC44B23 /,          JBLTYP(191)  / 0 /            
      DATA  CBLOCK(192)  / 6HTBTB2  /,          NCBARR(192)  / 73 /             
        DATA  CBLSER(192)  / 6HC44B24 /,          JBLTYP(192)  / 0 /            
      DATA  CBLOCK(193)  / 6HITBTB3 /,          NCBARR(193)  / 73 /             
        DATA  CBLSER(193)  / 6HC44B25 /,          JBLTYP(193)  / 0 /            
      DATA  CBLOCK(194)  / 6HTBTEXT /,          NCBARR(194)  / 73 /             
        DATA  CBLSER(194)  / 6HC44B26 /,          JBLTYP(194)  / 1 /            
      DATA  CBLOCK(195)  / 6HKARRAY /,          NCBARR(195)  /  9 /             
        DATA  CBLSER(195)  / 6HC45B01 /,          JBLTYP(195)  / 0 /            
      DATA  CBLOCK(196)  / 6HKARRAY /,          NCBARR(196)  /  9 /             
        DATA  CBLSER(196)  / 6HC47B01 /,          JBLTYP(196)  / 0 /            
      DATA  CHAR(1)   /  1HI  /                                                 
      DATA  CHAR(2)   /  1HJ  /                                                 
      DATA  CHAR(3)   /  1HK  /                                                 
      DATA  CHAR(4)   /  1HL  /                                                 
      DATA  CHAR(5)   /  1HM  /                                                 
      DATA  CHAR(6)   /  1HN  /                                                 
      DATA  TYPE(1,1)  / 6HREAL*8 /                                             
      DATA  TYPE(1,2)  / 6H       /                                             
      DATA  TYPE(1,3)  / 6H       /                                             
      DATA  TYPE(2,1)  / 6HCOMPLE /                                             
      DATA  TYPE(2,2)  / 6HX* 8   /                                             
      DATA  TYPE(2,3)  / 6H       /                                             
      DATA  TYPE(3,1)  / 6HREAL*8 /                                             
      DATA  TYPE(3,2)  / 6H       /                                             
      DATA  TYPE(3,3)  / 6H       /                                             
      DATA  TYPE(4,1)  / 6HINTEGE /                                             
      DATA  TYPE(4,2)  / 6HR*4    /                                             
      DATA  TYPE(4,3)  / 6H       /                                             
      NUMLST = 28                                                               
      LUNIT2 = 7                                                                
      LUNIT3 = 8                                                                
      LUNIT5 = 5                                                                
      LUNIT6 = 6                                                                
      MULVAR(1)  =  2                                                           
      MULVAR(2)  =  2                                                           
      MULVAR(3)  =  2                                                           
      MULVAR(4)  =  1                                                           
      OPEN ( UNIT = 7, TYPE = 'NEW', FORM = 'FORMATTED' )                       
      OPEN ( UNIT = 8, TYPE = 'NEW', FORM = 'FORMATTED' )                       
      INDEXM = 0                                                                
      MEXTRA = 0                                                                
C     ONE LESS THAN THE NUMBER OF  'VARDIM'  MODULES.                           
      NUMKEX = 7                                                                
C     DEFAULT VALUES FOR EXTRA OFFSETS OF  'VARDIM'                             
C     MODULES BELONGING TO  NON-SOLUTION OVERLAYS.                              
      DO 5245  I=1, NUMKEX                                                      
 5245 KEXTRA(I) = 0                                                             
C     DEFAULT VALUES FOR EMTP LIST SIZES FOLLOW.                                
      LSTDEF(1)  =  250                                                         
      LSTDEF(2)  =  300                                                         
      LSTDEF(3)  =  500                                                         
      LSTDEF(4)  =  100                                                         
      LSTDEF(5)  =  2500                                                        
      LSTDEF(6)  =  40                                                          
      LSTDEF(7)  =  550                                                         
      LSTDEF(8)  =  1750                                                        
      LSTDEF(9)  =  75                                                          
      LSTDEF(10)  =  160                                                        
      LSTDEF(11)  =  50                                                         
      LSTDEF(12)  =  50                                                         
      LSTDEF(13)  =  5                                                          
      LSTDEF(14)  =  460                                                        
      LSTDEF(15)  =  50                                                         
      LSTDEF(16)  =  40                                                         
      LSTDEF(17)  =  4                                                          
      LSTDEF(18)  =  5                                                          
      LSTDEF(19)  =  1600                                                       
      LSTDEF(20)  =  650                                                        
      LSTDEF(21)  =  100                                                        
      LSTDEF(22)  =  150                                                        
      LSTDEF(23)  =  4000                                                       
      LSTDEF(24)  =  3                                                          
      LSTDEF(25)  =  400                                                        
      LSTDEF(26)  =  50                                                         
      LSTDEF(27)  =  LSTDEF(11)                                                 
      LSTDEF(28)  =  1080                                                       
      READ (LUNIT5, 5287)  ( LSTNEW(I), I=1, NUMLST )                           
 5287 FORMAT ( 10I8 )                                                           
      WRITE (LUNIT6, 5263)                                                      
 5263 FORMAT ( /,  29H PSEUDO-LISTING OF DATA CARDS,                            
     1  38H WHICH HAVE BEEN READ BY THE VARIABLE-,                              
     2  39HDIMENSIONING PROGRAM  'VARDIM' .   ONLY  ,/,                         
     3  36H IF ALL DATA FIELDS ARE PUNCHED WITH,                                
     4  39H  'CLEAN'  I8  INTEGER INFORMATION WILL,                             
     5  37H THIS BE A TRUE LISTING.   DATA CARDS    ,/,                         
     6  41H ARE IN FACT READ IN AND THEN PRINTED OUT,                           
     7  34H USING INTEGER VARIABLES AND  10I8,                                  
     8   9H  FORMAT.                                )                           
      WRITE (LUNIT6, 5264)   ( I,  I=1, 8 ),                                    
     1                       ( LSTNEW(I), I=1, NUMLST )                         
 5264 FORMAT ( 1X,  111( 1H- )                      ,/,                         
     1  31X,  1H0,  8( 9X,  I1 )                    ,/,                         
     2  31X,  1H0,  8( 9X,  1H0 )                   ,/,                         
     3  31X,  80( 1H- )                             ,/,                         
     4  24H 1ST CARD (LISTS  1-10).,  7X,  1H1,  10I8 ,/,                       
     5  24H 2ND CARD (LISTS 11-20).,  7X,  1H1,  10I8 ,/,                       
     6  24H 3RD CARD (LISTS 21-30).,  7X,  1H1,  10I8 )                         
      IF ( LSTNEW(1) / 10000000  .NE.  9 )   GO TO 5294                         
      READ (LUNIT5, 5287)  ( KEXTRA(I), I=1, NUMKEX )                           
      WRITE (LUNIT6, 5378)  ( KEXTRA(I), I=1, NUMKEX )                          
 5378 FORMAT ( 22H SUPPLEMENTAL OFFSETS.,  9X,  1H1,                            
     1         10I8  )                                                          
      LSTNEW(1) = LSTNEW(1) - 90000000                                          
 5294 IF ( LSTNEW(11)/10000000 .NE. 9 ) GO TO 5297                              
      N4 = LSTNEW(11) - 90000000                                                
      DO 5296  J=1, NUMLST                                                      
 5296 LSTNEW(J) = LSTDEF(J) * N4                                                
 5297 IF ( LSTNEW(1)  .GT.  0 )                                                 
     1 LSTNEW(1) = LSTNEW(1) + 2                                                
      WRITE (LUNIT6, 5381)                                                      
 5381 FORMAT ( 1X,  111( 1H- )  )                                               
      DO 5326  I=1, NUMLST                                                      
      N1 = I                                                                    
      IF ( LSTNEW(I)  .GE.  1000000 )   GO TO 9000                              
      IF ( LSTNEW(I)  .LE.  0 )   LSTNEW(I) = LSTDEF(I)                         
 5326 CONTINUE                                                                  
      IF ( LSTNEW(19) .LE. 23 )  LSTNEW(19) = 23                                
      IF ( LSTNEW(26)  .LE.  10 )  LSTNEW(26) = 10                              
      N1 = LSTNEW(16) / 2                                                       
      IF ( 2*N1   .NE.   LSTNEW(16) )                                           
     1 LSTNEW(16) = LSTNEW(16) + 1                                              
      N1 = LSTNEW(27) / 2                                                       
      IF ( 2*N1  .NE.  LSTNEW(27) )                                             
     1 LSTNEW(27) = LSTNEW(27) + 1                                              
C     LIST NUMBER 51 IS A DEPENDENT LIST, ALWAYS TWICE                          
C     THE SIZE OF LIST NUMBER 13.                                               
C     THIS IS FOR FREQUENCY-DEPENDENCE ARRAYS                                   
C     'CON1' ,  'CON2' ,  AND  'CON3' .                                         
      LSTNEW(51)  =  6 * LSTNEW(13)                                             
C     LIST NUMBER 52 IS A DEPENDENT LIST, ALWAYS TWICE                          
C     THE SIZE OF LIST NUMBER 1, PLUS ONE.                                      
C     THIS IS FOR FREQUENCY-DEPENDENCE ARRAY   'IWTENT' .                       
      LSTNEW(52) = LSTNEW(51) + 1                                               
C     LIST NUMBER 7 ALSO SERVES FOR STORAGE AS PART OF                          
C     LIST NUMBER 3.   HENCE LIST 7 MUST NOT BE SHORTER.                        
C     IF (  LSTNEW(3)  .GT.  LSTNEW(7) )                                        
C    1 LSTNEW(7) = LSTNEW(3)                                                    
C     LIST NUMBER 53 IS FOR TERMINAL PAIRS                                      
C     ASSOCIATED WITH COMPENSATION ELEMENTS.                                    
      LSTNEW(53) = LSTNEW(9) + 3 * LSTNEW(17)                                   
C      CABLE ADDS 5 LIST-2 EXTENSIONS TO  'NAMEBR'                              
      LSTNEW(54) = 6 * LSTNEW(2)                                                
C     LIST 55 HAS ONE ENTRY FOR EACH SUBNETWORK.                                
      LSTNEW(55)  =  LSTNEW(9)  +  LSTNEW(17)                                   
C     LIST 56 IS FOR VLADIMIR'S "CIKFD" AND "RMFD"                              
C     ARRAYS FOR FREQ-DEPEND. GENERATOR EQUIVALENT                              
      LSTNEW(56) = LSTNEW(2) * LSTNEW(27) / 2                                   
C     LIST 57 MAPS #PHASES INTO ZNONL SIZE.                                     
      LSTNEW(57)  =  LSTNEW(1) * LSTNEW(24)                                     
C     LIST 58 IS FOR EXTREMA VECTOR  "XMAX" .                                   
      LSTNEW(58) = 4 * LSTNEW(12)                                               
C     LIST 59 IS EXTENDED WORKING VECTOR "VOLTI"                                
      LSTNEW(59) = 2 * LSTNEW(26)                                               
C     LIST 60 PROVIDES DOUBLE-LENGTH "KMSWIT" AS                                
C     REPLACEMENT FOR EARLIER "KLOW" AND "KHIGH"                                
      LSTNEW(60) = 3 * LSTNEW(6)                                                
C     LIST 61 IS FOR ONTARIO HYDRO FREQ-DEP SOURCE                              
      LSTNEW(61) = 1                                                            
C     LIST 62 IS FOR POWER/ENERGY ("KOUTVP" WHICH                               
C     NOW INCLUDES FORMER, SEPARATE "KOUTIE")                                   
      LSTNEW(62) = 2 * LSTNEW(18)                                               
      LSTNEW(63) = 3 * LSTNEW(6)                                                
      LSTNEW(64) = 4 * LSTNEW(6)                                                
C     LIST 65 IS FOR TYPE-59 S.M. ELECTRICAL DATA:                              
      LSTNEW(65) = 101 * LSTNEW(17)                                             
C     LIST 66 IS FOR TYPE-59 S.M. ELECTRICAL VARIABLES:                         
      LSTNEW(66) =  24 * LSTNEW(17)                                             
C     LIST 67 IS FOR TYPE-59 S.M. MECHANICAL DATA:                              
      LSTNEW(67) =  12 * LSTNEW(16)                                             
C     LIST 68 IS FOR TYPE-59 S.M. MECHANICAL VARIABLES:                         
      LSTNEW(68) =   6 * LSTNEW(16)                                             
C     LIST 69 IS FOR TYPE-59 S.M. POINTERS:                                     
      LSTNEW(69) =  30 * LSTNEW(17)                                             
C     LIST 70 IS FOR TYPE-59 S.M. OUTPUT POINTERS:                              
      LSTNEW(70) =   5 * LSTNEW(11) + 2                                         
C     LIST 71 IS TO EXTEND LIST 21 FOR LJG (18 AUG 1987):                       
      LSTNEW(71) =   2 * LSTNEW(21)                                             
      LSTNEW(72) =   LSTNEW(8) + LSTNEW(28)                                     
C     LIST 73 IS TO EXTEND LIST 22 FOR LJG (10 MAR 1988):                       
      LSTNEW(73) =   LSTNEW(22) + LSTNEW(21)                                    
      MTOT = 0                                                                  
      DO 4301  I =   1, 126                                                     
      N9 = NCBARR(I)                                                            
      IF ( N9  .EQ.  0     .OR.     N9  .EQ.  98 )   GO TO 4301                 
      N37 = 3                                                                   
      BUS1(1) = CBLOCK(I)                                                       
      DECODE (1, 8104, BUS1)  BUS2                                              
 8104 FORMAT ( A1 )                                                             
      DO 4701  J=1, 6                                                           
      IF ( BUS2  .EQ.  CHAR(J) )   N37 = 4                                      
 4701 CONTINUE                                                                  
      IF ( JBLTYP(I)  .NE.  0 )   N37 = JBLTYP(I)                               
      MTOT = MTOT + MULVAR(N37)*LSTNEW(N9)                                      
 4301 CONTINUE                                                                  
      WRITE (LUNIT2, 4201)                                                      
 4201 FORMAT ( 6X,  17HSUBROUTINE MAIN10,  57X  ,/,                             
     1  80H      IMPLICIT REAL*8 (A-H, O-Z),  INTEGER*4 (I-N)                   
     2                                                             )            
      DO 4101  II=  1, 126                                                      
      I =   0  +  II                                                            
      N3 = NCBARR(I)                                                            
      NONNEG = LSTNEW(N3)                                                       
      IF ( NONNEG  .LE.  0 )   NONNEG = 1                                       
      WRITE (LUNIT2, 7236)  CBLSER(I), CBLOCK(I), NONNEG                        
 7236 FORMAT ( 6X,  10HCOMMON  / ,  A6,  5H /   ,  A6,  1H(,                    
     1  I8,  2H ),  38X  )                                                      
      N4 = JBLTYP(I)                                                            
      IF ( N4  .EQ.  0 )   GO TO 4101                                           
      WRITE (LUNIT2, 7243)  (TYPE(N4,J), J=1, 3),  CBLOCK(I)                    
 7243 FORMAT ( 6X,  3A6,  A6,  50X )                                            
 4101 CONTINUE                                                                  
      WRITE (LUNIT2, 7286)                                                      
 7286 FORMAT ( 6X,  11HCALL SUBR10,  63X )                                      
      WRITE (LUNIT2, 7297)                                                      
 7297 FORMAT ( 6X,  6HRETURN,  68X )                                            
      WRITE (LUNIT2, 7298)                                                      
 7298 FORMAT ( 6X,  3HEND,  71X )                                               
      LTLABL = MTOT                                                             
      IF ( LTLABL  .GT.  9999999 )   GO TO 9000                                 
      INDEXM = INDEXM + 1                                                       
      MUL34 = MULVAR(3) / MULVAR(4)                                             
      MEXTRA = 4000                                                             
      LSTNEW(98) = LTLABL                                                       
      WRITE (LUNIT3, 8116)                                                      
 8116 FORMAT ( 6X,  47HSUBROUTINE DIMENS ( LSIZE, NCHAIN, BUS1, BUS2 ),         
     1  27X  ,/,                                                                
     2  32H      IMPLICIT REAL*8 (A-H, O-Z)   )                                 
      WRITE (LUNIT3, 8120)                                                      
 8120 FORMAT ( 6X,  20HDIMENSION  LSIZE(62),  54X )                             
      WRITE (LUNIT3, 8124)                                                      
 8124 FORMAT ( 6X,   24HREAL          BUS1, BUS2,  50X )                        
      WRITE (LUNIT3, 8134)                                                      
 8134 FORMAT ( 6X,  36HIF ( NCHAIN  .GE.  29 )   GO TO 2900,  38X )             
      DO 8137  I=1, NUMLST                                                      
 8137 WRITE (LUNIT3, 8143)  I, LSTNEW(I)                                        
 8143 FORMAT ( 6X,  6HLSIZE(,  I2,  4H)  =,  I8,  56X )                         
      WRITE (LUNIT3, 8155)  NUMLST                                              
 8155 FORMAT ( 6X,  4HN7 =,  I3,  4H + 1,  63X )                                
      WRITE (LUNIT3, 8159)  LTLABL                                              
 8159 FORMAT ( 6X,  11HLSIZE(N7) =,  I8,  55X )                                 
      NREC3 = NUMLST + 7                                                        
      CALL TIME44 ( TEXTA )                                                     
      CALL DATE44 ( TEXTB )                                                     
      WRITE (ANSI16, 3672)  TEXTA, TEXTB                                        
 3672 FORMAT ( 4A4 )                                                            
      READ (ANSI16, 3676) NH, NM, NS, NM, ND, NY                                
 3676 FORMAT ( I2, 1X, I2, 1X, 2I2, 1X, I2, 1X, I2 )                            
      NTIME = 10000*NH + 100*NM + NS                                            
      NDATE = 10000*NM + 100*ND + NY                                            
      WRITE (LUNIT3, 3684)  NTIME, NDATE                                        
 3684 FORMAT ( 6X, 6HBUS1 =,  I8  ,/,                                           
     1         6X, 6HBUS2 =,  I8  )                                             
      MTOT = 0                                                                  
      DO 4302  I = 127, 138                                                     
      N9 = NCBARR(I)                                                            
      IF ( N9  .EQ.  0     .OR.     N9  .EQ.  98 )   GO TO 4302                 
      N37 = 3                                                                   
      BUS1(1) = CBLOCK(I)                                                       
      DECODE (1, 8104, BUS1)  BUS2                                              
      DO 4702  J=1, 6                                                           
      IF ( BUS2  .EQ.  CHAR(J) )   N37 = 4                                      
 4702 CONTINUE                                                                  
      IF ( JBLTYP(I)  .NE.  0 )   N37 = JBLTYP(I)                               
      MTOT = MTOT + MULVAR(N37)*LSTNEW(N9)                                      
 4302 CONTINUE                                                                  
      LSTNEW(99) = LTLABL + KEXTRA( 1)                                          
      IF ( LSTNEW(99) .LE. 0 )  LSTNEW(99) = 1                                  
      NCBARR(127) = 99                                                          
      WRITE (LUNIT2, 4202)                                                      
 4202 FORMAT ( 6X,  17HSUBROUTINE OVER29,  57X  ,/,                             
     1  80H      IMPLICIT REAL*8 (A-H, O-Z),  INTEGER*4 (I-N)                   
     2                                                             )            
      WRITE (LUNIT3, 8156)                                                      
 8156 FORMAT ( 6X,  6HRETURN,  68X )                                            
      LM = 0                                                                    
      N86 = 29                                                                  
      N87 = 31                                                                  
      WRITE (LUNIT3, 8158)  N86, N86, N87                                       
      NREC3 = NREC3 + 2                                                         
 8158 FORMAT ( I3,  20H00 IF ( NCHAIN  .GT.,  I4,                               
     1  10H )   GO TO,  I3,   2H00,  38X )                                      
      DO 4102  II=  1,  12                                                      
      I = 126  +  II                                                            
      N3 = NCBARR(I)                                                            
      IF ( II  .EQ.  1 )   GO TO 4502                                           
      N7 = II - 1                                                               
      DO 4402  KK=1, N7                                                         
      IF ( NCBARR(KK+126)  .EQ.  N3 )   GO TO 4602                              
 4402 CONTINUE                                                                  
 4502 LM = LM + 1                                                               
      N28 = N3                                                                  
      IF ( N3  .EQ.  99 )   N28 = 0                                             
      WRITE (LUNIT3, 8143)  LM, N28                                             
      LM = LM + 1                                                               
      WRITE (LUNIT3, 4769)  LM, LSTNEW(N3)                                      
 4769 FORMAT ( 6X,  6HLSIZE(,  I2,  3H) =,  I7,  56X  )                         
      NREC3 = NREC3 + 2                                                         
 4602 CONTINUE                                                                  
      NONNEG = LSTNEW(N3)                                                       
      IF ( NONNEG  .LE.  0 )   NONNEG = 1                                       
      WRITE (LUNIT2, 7236)  CBLSER(I), CBLOCK(I), NONNEG                        
      N4 = JBLTYP(I)                                                            
      IF ( N4  .EQ.  0 )   GO TO 4102                                           
      WRITE (LUNIT2, 7243)  (TYPE(N4,J), J=1, 3),  CBLOCK(I)                    
 4102 CONTINUE                                                                  
      N18 = 29                                                                  
      WRITE (LUNIT2, 7274)  N18                                                 
 7274 FORMAT ( 6X,  9HCALL SUBR,  I2,  63X )                                    
      WRITE (LUNIT2, 7297)                                                      
      WRITE (LUNIT2, 7298)                                                      
      INDEXM = INDEXM + 1                                                       
      MEXTRA = 5000                                                             
      WRITE (LUNIT2, 4203)                                                      
 4203 FORMAT ( 6X,  17HSUBROUTINE OVER31,  57X  ,/,                             
     1  80H      IMPLICIT REAL*8 (A-H, O-Z),  INTEGER*4 (I-N)                   
     2                                                             )            
      WRITE (LUNIT3, 8156)                                                      
      LM = 0                                                                    
      N86 = 31                                                                  
      N87 = 39                                                                  
      WRITE (LUNIT3, 8158)  N86, N86, N87                                       
      NREC3 = NREC3 + 2                                                         
      DO 4103  II=  1,   1                                                      
      I = 138  +  II                                                            
      N3 = NCBARR(I)                                                            
      IF ( II  .EQ.  1 )   GO TO 4503                                           
      N7 = II - 1                                                               
      DO 4403  KK=1, N7                                                         
      IF ( NCBARR(KK+138)  .EQ.  N3 )   GO TO 4603                              
 4403 CONTINUE                                                                  
 4503 LM = LM + 1                                                               
      N28 = N3                                                                  
      IF ( N3  .EQ.  99 )   N28 = 0                                             
      WRITE (LUNIT3, 8143)  LM, N28                                             
      LM = LM + 1                                                               
      WRITE (LUNIT3, 4769)  LM, LSTNEW(N3)                                      
      NREC3 = NREC3 + 2                                                         
 4603 CONTINUE                                                                  
      NONNEG = LSTNEW(N3)                                                       
      IF ( NONNEG  .LE.  0 )   NONNEG = 1                                       
      WRITE (LUNIT2, 7236)  CBLSER(I), CBLOCK(I), NONNEG                        
      N4 = JBLTYP(I)                                                            
      IF ( N4  .EQ.  0 )   GO TO 4103                                           
      WRITE (LUNIT2, 7243)  (TYPE(N4,J), J=1, 3),  CBLOCK(I)                    
 4103 CONTINUE                                                                  
      N18 = 31                                                                  
      WRITE (LUNIT2, 7274)  N18                                                 
      WRITE (LUNIT2, 7297)                                                      
      WRITE (LUNIT2, 7298)                                                      
      LSTNEW(71) = 500                                                          
      IF ( LSTNEW(3) .GT. 500 )                                                 
     1 LSTNEW(71) = LSTNEW(3)                                                   
      WRITE (LUNIT2, 4204)                                                      
 4204 FORMAT ( 6X,  17HSUBROUTINE OVER39,  57X  ,/,                             
     1  80H      IMPLICIT REAL*8 (A-H, O-Z),  INTEGER*4 (I-N)                   
     2                                                             )            
      WRITE (LUNIT3, 8156)                                                      
      LM = 0                                                                    
      N86 = 39                                                                  
      N87 = 10                                                                  
      WRITE (LUNIT3, 8158)  N86, N86, N87                                       
      NREC3 = NREC3 + 2                                                         
      DO 4104  II=  1,   3                                                      
      I = 139  +  II                                                            
      N3 = NCBARR(I)                                                            
      IF ( II  .EQ.  1 )   GO TO 4504                                           
      N7 = II - 1                                                               
      DO 4404  KK=1, N7                                                         
      IF ( NCBARR(KK+139)  .EQ.  N3 )   GO TO 4604                              
 4404 CONTINUE                                                                  
 4504 LM = LM + 1                                                               
      N28 = N3                                                                  
      IF ( N3  .EQ.  99 )   N28 = 0                                             
      WRITE (LUNIT3, 8143)  LM, N28                                             
      LM = LM + 1                                                               
      WRITE (LUNIT3, 4769)  LM, LSTNEW(N3)                                      
      NREC3 = NREC3 + 2                                                         
 4604 CONTINUE                                                                  
      NONNEG = LSTNEW(N3)                                                       
      IF ( NONNEG  .LE.  0 )   NONNEG = 1                                       
      WRITE (LUNIT2, 7236)  CBLSER(I), CBLOCK(I), NONNEG                        
      N4 = JBLTYP(I)                                                            
      IF ( N4  .EQ.  0 )   GO TO 4104                                           
      WRITE (LUNIT2, 7243)  (TYPE(N4,J), J=1, 3),  CBLOCK(I)                    
 4104 CONTINUE                                                                  
      N18 = 39                                                                  
      WRITE (LUNIT2, 7274)  N18                                                 
      WRITE (LUNIT2, 7297)                                                      
      WRITE (LUNIT2, 7298)                                                      
      WRITE (LUNIT2, 4205)                                                      
 4205 FORMAT ( 6X,  17HSUBROUTINE FIXS10,  57X  ,/,                             
     1  80H      IMPLICIT REAL*8 (A-H, O-Z),  INTEGER*4 (I-N)                   
     2                                                             )            
      WRITE (LUNIT3, 8156)                                                      
      LM = 0                                                                    
      N86 = 10                                                                  
      N87 = 44                                                                  
      WRITE (LUNIT3, 8158)  N86, N86, N87                                       
      NREC3 = NREC3 + 2                                                         
      DO 4105  II=  1,  26                                                      
      I = 142  +  II                                                            
      N3 = NCBARR(I)                                                            
      IF ( II  .EQ.  1 )   GO TO 4505                                           
      N7 = II - 1                                                               
      DO 4405  KK=1, N7                                                         
      IF ( NCBARR(KK+142)  .EQ.  N3 )   GO TO 4605                              
 4405 CONTINUE                                                                  
 4505 LM = LM + 1                                                               
      N28 = N3                                                                  
      IF ( N3  .EQ.  99 )   N28 = 0                                             
      WRITE (LUNIT3, 8143)  LM, N28                                             
      LM = LM + 1                                                               
      WRITE (LUNIT3, 4769)  LM, LSTNEW(N3)                                      
      NREC3 = NREC3 + 2                                                         
 4605 CONTINUE                                                                  
      NONNEG = LSTNEW(N3)                                                       
      IF ( NONNEG  .LE.  0 )   NONNEG = 1                                       
      WRITE (LUNIT2, 7236)  CBLSER(I), CBLOCK(I), NONNEG                        
      N4 = JBLTYP(I)                                                            
      IF ( N4  .EQ.  0 )   GO TO 4105                                           
      WRITE (LUNIT2, 7243)  (TYPE(N4,J), J=1, 3),  CBLOCK(I)                    
 4105 CONTINUE                                                                  
      N18 = 10                                                                  
      WRITE (LUNIT2, 7274)  N18                                                 
      WRITE (LUNIT2, 7297)                                                      
      WRITE (LUNIT2, 7298)                                                      
      INDEXM = INDEXM + 1                                                       
      MEXTRA = 7000                                                             
      D1 = ( LTLABL + MEXTRA + KEXTRA(INDEXM) ) / MUL34                         
      D2 = ( 91./4. + 19./2. ) ** 2                                             
      D3 = 4. * ( 11./8. + 19./2. ) * ( 9. - D1 )                               
      D1 = SQRT ( D2 - D3 )                                                     
      LPHASE = (-(91./4.+19./2.)+D1)/(2.*(11./8.+19./2.))                       
      IF ( LPHASE  .LE.  1 )   LPHASE = 2                                       
      IF ( LPHASE  .GT.  100 )   LPHASE = LPHASE * 0.7                          
      LPHD2 = LPHASE / 2                                                        
      LSTNEW(71) = LPHD2 * 2                                                    
      LSTNEW(73) = LSTNEW(71) + 1                                               
      LSTNEW(74) = ( LPHD2 * ( LPHD2+1) ) / 2                                   
      LSTNEW(75) = ( LSTNEW(71) * ( LSTNEW(71)+1) ) / 2                         
      LSTNEW(76) = 2 * LSTNEW(71)                                               
      WRITE (LUNIT2, 4206)                                                      
 4206 FORMAT ( 6X,  17HSUBROUTINE OVER44,  57X  ,/,                             
     1  80H      IMPLICIT REAL*8 (A-H, O-Z),  INTEGER*4 (I-N)                   
     2                                                             )            
      WRITE (LUNIT3, 8156)                                                      
      LM = 0                                                                    
      N86 = 44                                                                  
      N87 = 45                                                                  
      WRITE (LUNIT3, 8158)  N86, N86, N87                                       
      NREC3 = NREC3 + 2                                                         
      DO 4106  II=  1,  26                                                      
      I = 168  +  II                                                            
      N3 = NCBARR(I)                                                            
      IF ( II  .EQ.  1 )   GO TO 4506                                           
      N7 = II - 1                                                               
      DO 4406  KK=1, N7                                                         
      IF ( NCBARR(KK+168)  .EQ.  N3 )   GO TO 4606                              
 4406 CONTINUE                                                                  
 4506 LM = LM + 1                                                               
      N28 = N3                                                                  
      IF ( N3  .EQ.  99 )   N28 = 0                                             
      WRITE (LUNIT3, 8143)  LM, N28                                             
      LM = LM + 1                                                               
      WRITE (LUNIT3, 4769)  LM, LSTNEW(N3)                                      
      NREC3 = NREC3 + 2                                                         
 4606 CONTINUE                                                                  
      NONNEG = LSTNEW(N3)                                                       
      IF ( NONNEG  .LE.  0 )   NONNEG = 1                                       
      WRITE (LUNIT2, 7236)  CBLSER(I), CBLOCK(I), NONNEG                        
      N4 = JBLTYP(I)                                                            
      IF ( N4  .EQ.  0 )   GO TO 4106                                           
      WRITE (LUNIT2, 7243)  (TYPE(N4,J), J=1, 3),  CBLOCK(I)                    
 4106 CONTINUE                                                                  
      N18 = 44                                                                  
      WRITE (LUNIT2, 7274)  N18                                                 
      WRITE (LUNIT2, 7297)                                                      
      WRITE (LUNIT2, 7298)                                                      
      INDEXM = INDEXM + 1                                                       
      MEXTRA = 5000                                                             
      WRITE (LUNIT2, 4207)                                                      
 4207 FORMAT ( 6X,  17HSUBROUTINE OVER45,  57X  ,/,                             
     1  80H      IMPLICIT REAL*8 (A-H, O-Z),  INTEGER*4 (I-N)                   
     2                                                             )            
      WRITE (LUNIT3, 8156)                                                      
      LM = 0                                                                    
      N86 = 45                                                                  
      N87 = 47                                                                  
      WRITE (LUNIT3, 8158)  N86, N86, N87                                       
      NREC3 = NREC3 + 2                                                         
      DO 4107  II=  1,   1                                                      
      I = 194  +  II                                                            
      N3 = NCBARR(I)                                                            
      IF ( II  .EQ.  1 )   GO TO 4507                                           
      N7 = II - 1                                                               
      DO 4407  KK=1, N7                                                         
      IF ( NCBARR(KK+194)  .EQ.  N3 )   GO TO 4607                              
 4407 CONTINUE                                                                  
 4507 LM = LM + 1                                                               
      N28 = N3                                                                  
      IF ( N3  .EQ.  99 )   N28 = 0                                             
      WRITE (LUNIT3, 8143)  LM, N28                                             
      LM = LM + 1                                                               
      WRITE (LUNIT3, 4769)  LM, LSTNEW(N3)                                      
      NREC3 = NREC3 + 2                                                         
 4607 CONTINUE                                                                  
      NONNEG = LSTNEW(N3)                                                       
      IF ( NONNEG  .LE.  0 )   NONNEG = 1                                       
      WRITE (LUNIT2, 7236)  CBLSER(I), CBLOCK(I), NONNEG                        
      N4 = JBLTYP(I)                                                            
      IF ( N4  .EQ.  0 )   GO TO 4107                                           
      WRITE (LUNIT2, 7243)  (TYPE(N4,J), J=1, 3),  CBLOCK(I)                    
 4107 CONTINUE                                                                  
      N18 = 45                                                                  
      WRITE (LUNIT2, 7274)  N18                                                 
      WRITE (LUNIT2, 7297)                                                      
      WRITE (LUNIT2, 7298)                                                      
      INDEXM = INDEXM + 1                                                       
      MEXTRA = 5000                                                             
      WRITE (LUNIT2, 4208)                                                      
 4208 FORMAT ( 6X,  17HSUBROUTINE OVER47,  57X  ,/,                             
     1  80H      IMPLICIT REAL*8 (A-H, O-Z),  INTEGER*4 (I-N)                   
     2                                                             )            
      WRITE (LUNIT3, 8156)                                                      
      LM = 0                                                                    
      N86 = 47                                                                  
      N87 = 99                                                                  
      WRITE (LUNIT3, 8158)  N86, N86, N87                                       
      NREC3 = NREC3 + 2                                                         
      DO 4108  II=  1,   1                                                      
      I = 195  +  II                                                            
      N3 = NCBARR(I)                                                            
      IF ( II  .EQ.  1 )   GO TO 4508                                           
      N7 = II - 1                                                               
      DO 4408  KK=1, N7                                                         
      IF ( NCBARR(KK+195)  .EQ.  N3 )   GO TO 4608                              
 4408 CONTINUE                                                                  
 4508 LM = LM + 1                                                               
      N28 = N3                                                                  
      IF ( N3  .EQ.  99 )   N28 = 0                                             
      WRITE (LUNIT3, 8143)  LM, N28                                             
      LM = LM + 1                                                               
      WRITE (LUNIT3, 4769)  LM, LSTNEW(N3)                                      
      NREC3 = NREC3 + 2                                                         
 4608 CONTINUE                                                                  
      NONNEG = LSTNEW(N3)                                                       
      IF ( NONNEG  .LE.  0 )   NONNEG = 1                                       
      WRITE (LUNIT2, 7236)  CBLSER(I), CBLOCK(I), NONNEG                        
      N4 = JBLTYP(I)                                                            
      IF ( N4  .EQ.  0 )   GO TO 4108                                           
      WRITE (LUNIT2, 7243)  (TYPE(N4,J), J=1, 3),  CBLOCK(I)                    
 4108 CONTINUE                                                                  
      N18 = 47                                                                  
      WRITE (LUNIT2, 7274)  N18                                                 
      WRITE (LUNIT2, 7297)                                                      
      WRITE (LUNIT2, 7298)                                                      
      WRITE (LUNIT3, 8156)                                                      
      WRITE (LUNIT3, 8234)                                                      
 8234 FORMAT (44H 9900 LSIZE(1) = LOCINT(BUS1) - LOCINT(BUS2),  36X )           
      WRITE (LUNIT3, 8156)                                                      
      WRITE (LUNIT3, 8242)                                                      
 8242 FORMAT ( 6X,  3HEND,  71X )                                               
      NREC3 = NREC3 + 4                                                         
      REWIND LUNIT3                                                             
      DO 8258  M=1, 99999                                                       
      READ (LUNIT3, 8253, END=8259)  ABUFF                                      
 8253 FORMAT ( 13A6,  A2 )                                                      
 8258 WRITE (LUNIT2, 8253)  ABUFF                                               
 8259 CONTINUE                                                                  
      WRITE (LUNIT6, 8942)  LTLABL                                              
 8942 FORMAT ( 26H NORMAL TERMINATION WITHIN,                                   
     1  18H  'VARDIM' .   THE                       ,/,                         
     2  27H SIZE OF   /LABEL/   EQUALS,  I8,                                    
     3  17H   INTEGER WORDS.  ,//,  1X              )                           
      GO TO 9999                                                                
 9000 WRITE (LUNIT6, 9023)                                                      
 9023 FORMAT ( //, 1X  )                                                        
      WRITE (LUNIT6, 9027)                                                      
 9027 FORMAT ( 1X,  131( 1H- )  )                                               
      WRITE (LUNIT6, 9031)                                                      
 9031 FORMAT (  22( 6H ERROR )   )                                              
      WRITE (LUNIT6, 9031)                                                      
      WRITE (LUNIT6, 9027)                                                      
      WRITE (LUNIT6, 9044)                                                      
 9044 FORMAT ( /, 5X,   23HTHE USER IS IN TROUBLE,,                             
     1  42H UNLESS HE HAS MADE A DATA-PUNCHING ERROR.,                          
     2  40H   VARIABLE-DIMENSIONING OF THE EMTP HAS   ,/,                       
     3  5X,  40HBROKEN DOWN WITHIN THE SEPARATE VARIABLE,                       
     4  38H-DIMENSIONING PROGRAM  'VARDIM' .   IN,                              
     5  31H PARTICULAR, ONE OR MORE OF THE          ,/,                         
     6  5X,  32HUSER-INPUTTED LIST SIZES MUST BE,                               
     7  43H REJECTED AS BEING ILLEGALLY LARGE.   USER-,                         
     8  35HSUPPLIED LIMITS (OR DEFAULT LIMITS,      )                           
      WRITE (LUNIT6, 9045)   ( LSTNEW(I), I=1, NUMLST )                         
 9045 FORMAT (  5X,  25HFOR ANY NON-POSITIVE DATA,                              
     1  28H FIELDS) ARE AS FOLLOWS ....   ,/,                                   
     2 ( 1X,  10I10 )  )                                                        
      IF ( LTLABL  .GT.  0 )   GO TO 9061                                       
      WRITE (LUNIT6, 9049)  N1, LSTNEW(N1)                                      
 9049 FORMAT ( 5X,  23HSPECIFICALLY, THE USER-,                                 
     1  38HSUPPLIED VALUE WHICH WAS READ FOR LIST,                              
     2   7H NUMBER,  I4,  19H  EXCEEDS  999999 ,     ,/,                        
     3  5X,  29HWHICH IS UNREAL.   A VALUE OF,  I10,                            
     4  37H   WAS READ FROM THE USER'S DATA CARD,                               
     5  15H FOR THIS LIST.    ,/,  1X               )                           
 9054 WRITE (LUNIT6, 9027)                                                      
      WRITE (LUNIT6, 9031)                                                      
      WRITE (LUNIT6, 9031)                                                      
      WRITE (LUNIT6, 9027)                                                      
      WRITE (LUNIT6, 9023)                                                      
      GO TO 9900                                                                
 9061 WRITE (LUNIT6, 9064)  LTLABL                                              
 9064 FORMAT ( 5X, 31HSPECIFICALLY, THE USER-SUPPLIED,                          
     1  40H DIMENSIONS HAVE COLLECTIVELY PRODUCED A,                            
     2  32H TOTAL LABELED-COMMON ALLOCATION         ,/,                         
     3  5X,   2HOF,  I12,  26H   WORDS, WHICH IS UNREAL.,                       
     4  41H   A LIMIT OF   9999999   HAS ARBITRARILY,                           
     5  36H BEEN IMPOSED BY THE BETTER JUDGMENT     )                           
      WRITE (LUNIT6, 9065)                                                      
 9065 FORMAT ( 5X, 32HOF THE PROGRAM.   A MACHINE THIS,                         
     1  41H BIG (WITH THIS MUCH REAL CENTRAL MEMORY),                           
     2  41H IS NOT KNOWN TO BE IN EXISTENCE ANYWHERE  ,/,                       
     3  5X,  13HIN THE WORLD.              ,/,  1X  )                           
      GO TO 9054                                                                
 9900 CONTINUE                                                                  
      CLOSE ( UNIT=7, DISP='DELETE' )                                           
      CLOSE ( UNIT=8, DISP='DELETE' )                                           
 9999 STOP                                                                      
      END                                                                       
