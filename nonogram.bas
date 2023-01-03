   10 PRINT "CREATE AND TEST RANDOM NONOGROM (Y/N)";:INPUT A$
   20 IF A$="Y" THEN W%=5:H%=5:GOSUB 310:GOSUB 560:GOSUB 1090:GOSUB 520:GOSUB 800:END
   21 PRINT "CREATE AND TEST WIKI EXAMPLE (Y/N)";:INPUT A$
   22 IF A$="Y" THEN GOSUB 60:GOSUB 1090:GOSUB 520:GOSUB 800:END
   30 END
   40
   50 REM Set up a "P" nonogram (from the wiki page - https://en.wikipedia.org/wiki/Nonogram#Example)
   60 W%=8:H%=11:GOSUB 310
   70 REM Set up rows
   80 n%(1,1)=0
   90 n%(1,2)=1:l%(1,2,1)=4
  100 n%(1,3)=1:l%(1,3,1)=6
  110 n%(1,4)=2:l%(1,4,1)=2:l%(1,4,2)=2
  120 n%(1,5)=2:l%(1,5,1)=2:l%(1,5,2)=2
  130 n%(1,6)=1:l%(1,6,1)=6
  140 n%(1,7)=1:l%(1,7,1)=4
  150 n%(1,8)=1:l%(1,8,1)=2
  160 n%(1,9)=1:l%(1,9,1)=2
  170 n%(1,10)=1:l%(1,10,1)=2
  180 n%(1,11)=0
  190 REM Set up columns
  200 n%(2,1)=0
  210 n%(2,2)=1:l%(2,2,1)=9
  220 n%(2,3)=1:l%(2,3,1)=9
  230 n%(2,4)=2:l%(2,4,1)=2:l%(2,4,2)=2
  240 n%(2,5)=2:l%(2,5,1)=2:l%(2,5,2)=2
  250 n%(2,6)=1:l%(2,6,1)=4
  260 n%(2,7)=1:l%(2,7,1)=4
  270 n%(2,8)=0
  280 RETURN
  290
  300 REM PrepareData
  310 REM Maximum dimension (width or height)
  320 S%=W%:IF H%>W% THEN S%=H%
  330 REM All cells in g%(,) can be 0 (unknown), 1 (empty) or 2 (full)
  340 DIM g%(W%+1,H%+1)
  350 REM All nonogram line data l%(a,b,c) where a=1 or 2 (row or column), b=row/column index and c=nonogram run data
  360 DIM l%(2,S%,(S%+1)/2)
  370 REM Relates to l%(a,b,c) and stores number of entries for c
  380 DIM n%(2,S%)
  390 REM Stores all possible gap permutations for a row or column
  400 DIM gap((S%+1)/2+1)
  410 REM Stores all possible permutations for each row and column
  420 DIM perm$(2,S%,50):REM This will blow up if any row or column has more than 100 permutations
  430 REM Permutation count for each row and column
  440 DIM permutations(2,S%)
  450 REM Grid for solving in
  460 DIM s%(W%,H%)
  470 REM Line used for solving
  480 DIM v%(S%)
  490 RETURN
  500
  510 REM Transfer
  520 FOR Y%=1 TO H%:FOR X%=1 TO W%:g%(X%,Y%)=s%(X%,Y%):NEXT:NEXT
  530 RETURN
  540
  550 REM Create
  560 FOR Y%=1 TO H%:FOR X%=1 TO W%:g%(X%,Y%)=RND(2):s%(X%,Y%)=0:NEXT:NEXT
  570 FOR Y%=1 TO H%:t=1:i=Y%:GOSUB 620:NEXT
  580 FOR X%=1 TO W%:t=2:i=X%:GOSUB 620:NEXT
  590 RETURN
  600
  610 REM CreateLine(t,i)
  620 IF t=1 THEN x=1:y=i:dx=1:dy=0 ELSE x=i:y=1:dx=0:dy=1
  630 l=1
  640 IF x>W% OR y>H% OR g%(x,y)<>1 THEN GOTO 670
  650 x=x+dx:y=y+dy
  660 GOTO 640
  670 IF x>W% OR y>H% THEN GOTO 750
  680 k=0
  690 IF x>W% OR y>H% OR g%(x,y)<>2 THEN GOTO 730
  700 x=x+dx:y=y+dy
  710 k=k+1
  720 GOTO 690
  730 l%(t,i,l)=k
  740 l=l+1
  750 IF x<=W% AND y<=H% THEN GOTO 640
  760 n%(t,i)=l-1
  770 RETURN
  780
  790 REM Display
  800 L%=0
  810 FOR X%=1 TO W%
  820   IF n%(2,X%)>L% THEN L%=n%(2,X%)
  830 NEXT
  840 FOR J%=L% TO 1 STEP -1
  850   FOR X%=1 TO W%
  860     IF n%(2,X%)>=J% THEN PRINT "  ";l%(2,X%,J%);" "; ELSE PRINT "    ";
  870   NEXT
  880   PRINT
  890 NEXT
  900 FOR Y%=1 TO H%
  910   FOR X%=1 TO W%
  920     PRINT "+---";
  930   NEXT
  940   PRINT "+"
  950   FOR X%=1 TO W%
  960     IF g%(X%,Y%)=2 THEN PRINT "| * "; ELSE IF g%(X%,Y%)=1 THEN PRINT "|   "; ELSE PRINT "| ? ";
  970   NEXT
  980   PRINT "|";
  990   IF n%(1,Y%)>0 THEN FOR J%=1 TO n%(1,Y%):PRINT " ";l%(1,Y%,J%);:NEXT
 1000   PRINT
 1010 NEXT
 1020 FOR X%=1 TO W%
 1030   PRINT "+---";
 1040 NEXT
 1050 PRINT "+"
 1060 RETURN
 1070
 1080 REM Solve
 1090 FOR t=1 TO 2
 1100   IF t=1 THEN m=H%:totalGap=W% ELSE m=W%:totalGap=H%
 1110   FOR i=1 TO m
 1120     PRINT "Getting permutations for ";t;",";i
 1130     GOSUB 1850
 1140   NEXT
 1150 NEXT
 1160 solveCount=W%*H%
 1170 REPEAT
 1180   GOSUB 1240
 1190 UNTIL solveCount=0 OR canSolve=0
 1200 VDU 10
 1210 RETURN
 1220
 1230 REM Scan
 1240 canSolve=0
 1250 t=1
 1260 m=H%:totalGap=W%
 1270 PRINT "Setting commonalities for rows"
 1280 GOSUB 1440
 1290 t=2
 1300 m=W%:totalGap=H%
 1310 IF canSolve=0 THEN GOTO 1340
 1320 PRINT "Removing non-matching lines for columns"
 1330 GOSUB 1640
 1340 PRINT "Setting commonalities for columns"
 1350 GOSUB 1440
 1360 t=1
 1370 m=H%:totalGap=W%
 1380 IF canSolve=0 THEN GOTO 1410
 1390 PRINT "Removing non-matching lines for rows"
 1400 GOSUB 1640
 1410 RETURN
 1420
 1430 REM SetCommonalities
 1440 FOR i=1 TO m
 1450   IF t=1 THEN x=1:y=i:dx=1:dy=0 ELSE x=i:y=1:dx=0:dy=1
 1460   FOR k=1 TO totalGap
 1470     v$=STR$(s%(x,y))
 1480     IF v$<>"0" THEN GOTO 1580
 1490     valid=1
 1500     v$=MID$(perm$(t,i,1),k,1)
 1510     IF permutations(t,i)<2 THEN GOTO 1570
 1520     j=2
 1530     IF j>permutations(t,i) OR valid<>1 THEN GOTO 1570
 1540     IF v$<>MID$(perm$(t,i,j),k,1) THEN valid=0
 1550     j=j+1
 1560     GOTO 1530
 1570     IF valid=1 THEN s%(x,y)=VAL(v$):canSolve=1:solveCount=solveCount-1
 1580     x=x+dx:y=y+dy
 1590   NEXT
 1600 NEXT
 1610 RETURN
 1620
 1630 REM RemoveNonMatchingLines
 1640 FOR i=1 TO m
 1650   IF t=1 THEN x=1:y=i:dx=1:dy=0 ELSE x=i:y=1:dx=0:dy=1
 1660   FOR k=1 TO totalGap
 1670     v$=STR$(s%(x,y))
 1680     IF v$="0" THEN GOTO 1740
 1690     j=permutations(t,i)
 1700     IF j<1 THEN GOTO 1740
 1710     IF v$<>MID$(perm$(t,i,j),k,1) THEN GOSUB 1800
 1720     j=j-1
 1730     GOTO 1700
 1740     x=x+dx:y=y+dy
 1750   NEXT
 1760 NEXT
 1770 RETURN
 1780
 1790 REM RemoveLine
 1800 IF j<permutations(t,i) THEN FOR tmp=j+1 TO permutations(t,i):perm$(t,i,tmp-1)=perm$(t,i,tmp):NEXT
 1810 permutations(t,i)=permutations(t,i)-1
 1820 RETURN
 1830
 1840 REM GetGapPermutations
 1850 gCount=n%(t,i)+1
 1860 FOR j=1 TO gCount:gap(j)=0:NEXT
 1870 gapSize=totalGap
 1880 IF gCount>1 THEN FOR j=1 TO gCount-1:gapSize=gapSize-l%(t,i,j):NEXT
 1890 IF gCount>2 THEN gapSize=gapSize-(gCount-2)
 1900 gapTally=0
 1910 permutationCount=0
 1920 REPEAT
 1930   gap(gCount)=gapSize-gapTally
 1940   GOSUB 2070
 1950   j=0
 1960   REPEAT
 1970     j=j+1
 1980     gap(j)=gap(j)+1
 1990     gapTally=gapTally+1
 2000     IF gapTally>gapSize THEN gapTally=gapTally-gap(j):gap(j)=0
 2010   UNTIL j=gCount OR gap(j)>0
 2020 UNTIL j=gCount
 2030 permutations(t,i)=permutationCount
 2040 RETURN
 2050
 2060 REM AddPermutation
 2070 permutationCount=permutationCount+1
 2080 t$=""
 2090 byte$="1":num=gap(1):GOSUB 2170
 2100 IF gCount>1 byte$="2":num=l%(t,i,1):GOSUB 2170
 2110 IF gCount>2 FOR tmp=2 TO gCount-1:byte$="1":num=gap(tmp)+1:GOSUB 2170:byte$="2":num=l%(t,i,tmp):GOSUB 2170:NEXT
 2120 byte$="1":num=gap(gCount):GOSUB 2170
 2130 perm$(t,i,permutationCount)=t$
 2140 RETURN
 2150
 2160 REM RepeatByte(byte$,num)
 2170 IF num>0 THEN FOR temp=1 TO num:t$=t$+byte$:NEXT
 2180 RETURN
