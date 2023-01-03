   10 GOSUB 100
   20 GOSUB 370
   30 GOSUB 610
   40 GOSUB 890
   50 GOSUB 330
   60 GOSUB 610
   70 END
   80
   90 REM PrepareData
  100 REM Dimensions of nonogram
  110 W%=5:H%=5
  120 REM Maximum dimension (width or height)
  130 S%=W%:IF H%>W% THEN S%=H%
  140 REM All cells in g%(,) can be 0 (unknown), 1 (empty) or 2 (full)
  150 DIM g%(W%+1,H%+1)
  160 REM All nonogram line data l%(a,b,c) where a=1 or 2 (row or column), b=row/column index and c=nonogram run data
  170 DIM l%(2,S%,(S%+1)/2)
  180 REM Relates to l%(a,b,c) and stores number of entries for c
  190 DIM n%(2,S%)
  200 REM Stores all possible gap permutations for a row or column
  210 DIM gap((S%+1)/2+1)
  220 REM Stores all possible permutations for each row and column
  230 DIM perm$(2,S%,50):REM This will blow up if any row or column has more than 100 permutations
  240 REM Permutation count for each row and column
  250 DIM permutations(2,S%)
  260 REM Grid for solving in
  270 DIM s%(W%,H%)
  280 REM Line used for solving
  290 DIM v%(S%)
  300 RETURN
  310
  320 REM Transfer
  330 FOR Y%=1 TO H%:FOR X%=1 TO W%:g%(X%,Y%)=s%(X%,Y%):NEXT:NEXT
  340 RETURN
  350
  360 REM Create
  370 FOR Y%=1 TO H%:FOR X%=1 TO W%:g%(X%,Y%)=RND(2):s%(X%,Y%)=0:NEXT:NEXT
  380 FOR Y%=1 TO H%:t=1:i=Y%:GOSUB 430:NEXT
  390 FOR X%=1 TO W%:t=2:i=X%:GOSUB 430:NEXT
  400 RETURN
  410
  420 REM CreateLine(t,i)
  430 IF t=1 THEN x=1:y=i:dx=1:dy=0 ELSE x=i:y=1:dx=0:dy=1
  440 l=1
  450 IF x>W% OR y>H% OR g%(x,y)<>1 THEN GOTO 480
  460 x=x+dx:y=y+dy
  470 GOTO 450
  480 IF x>W% OR y>H% THEN GOTO 560
  490 k=0
  500 IF x>W% OR y>H% OR g%(x,y)<>2 THEN GOTO 540
  510 x=x+dx:y=y+dy
  520 k=k+1
  530 GOTO 500
  540 l%(t,i,l)=k
  550 l=l+1
  560 IF x<=W% AND y<=H% THEN GOTO 450
  570 n%(t,i)=l-1
  580 RETURN
  590
  600 REM Display
  610 FOR Y%=1 TO H%
  620   FOR X%=1 TO W%
  630     PRINT "+---";
  640   NEXT
  650   PRINT "+"
  660   FOR X%=1 TO W%
  670     IF g%(X%,Y%)=2 THEN PRINT "| * "; ELSE IF g%(X%,Y%)=1 THEN PRINT "|   "; ELSE PRINT "| ? ";
  680   NEXT
  690   PRINT "|";
  700   IF n%(1,Y%)>0 THEN FOR J%=1 TO n%(1,Y%):PRINT " ";l%(1,Y%,J%);:NEXT
  710   PRINT
  720 NEXT
  730 FOR X%=1 TO W%
  740   PRINT "+---";
  750 NEXT
  760 PRINT "+"
  770 C%=1:J%=1
  780 IF C%<>1 THEN GOTO 860
  790 C%=0
  800 FOR X%=1 TO W%
  810   IF n%(2,X%)>=J% THEN C%=1:PRINT "  ";l%(2,X%,J%);" "; ELSE PRINT "    ";
  820 NEXT
  830 PRINT
  840 J%=J%+1
  850 GOTO 780
  860 RETURN
  870
  880 REM Solve
  890 FOR t=1 TO 2
  900   IF t=1 THEN m=H%:totalGap=W% ELSE m=W%:totalGap=H%
  910   FOR i=1 TO m
  920     PRINT "Getting permutations for ";t;",";i
  930     GOSUB 1650
  940   NEXT
  950 NEXT
  960 solveCount=W%*H%
  970 REPEAT
  980   GOSUB 1040
  990 UNTIL solveCount=0 OR canSolve=0
 1000 VDU 10
 1010 RETURN
 1020
 1030 REM Scan
 1040 canSolve=0
 1050 t=1
 1060 m=H%:totalGap=W%
 1070 PRINT "Setting commonalities for rows"
 1080 GOSUB 1240
 1090 t=2
 1100 m=W%:totalGap=H%
 1110 IF canSolve=0 THEN GOTO 1140
 1120 PRINT "Removing non-matching lines for columns"
 1130 GOSUB 1440
 1140 PRINT "Setting commonalities for columns"
 1150 GOSUB 1240
 1160 t=1
 1170 m=H%:totalGap=W%
 1180 IF canSolve=0 THEN GOTO 1210
 1190 PRINT "Removing non-matching lines for rows"
 1200 GOSUB 1440
 1210 RETURN
 1220
 1230 REM SetCommonalities
 1240 FOR i=1 TO m
 1250   IF t=1 THEN x=1:y=i:dx=1:dy=0 ELSE x=i:y=1:dx=0:dy=1
 1260   FOR k=1 TO totalGap
 1270     v$=STR$(s%(x,y))
 1280     IF v$<>"0" THEN GOTO 1380
 1290     valid=1
 1300     v$=MID$(perm$(t,i,1),k,1)
 1310     IF permutations(t,i)<2 THEN GOTO 1370
 1320     j=2
 1330     IF j>permutations(t,i) OR valid<>1 THEN GOTO 1370
 1340     IF v$<>MID$(perm$(t,i,j),k,1) THEN valid=0
 1350     j=j+1
 1360     GOTO 1330
 1370     IF valid=1 THEN s%(x,y)=VAL(v$):canSolve=1:solveCount=solveCount-1
 1380     x=x+dx:y=y+dy
 1390   NEXT
 1400 NEXT
 1410 RETURN
 1420
 1430 REM RemoveNonMatchingLines
 1440 FOR i=1 TO m
 1450   IF t=1 THEN x=1:y=i:dx=1:dy=0 ELSE x=i:y=1:dx=0:dy=1
 1460   FOR k=1 TO totalGap
 1470     v$=STR$(s%(x,y))
 1480     IF v$="0" THEN GOTO 1540
 1490     j=permutations(t,i)
 1500     IF j<1 THEN GOTO 1540
 1510     IF v$<>MID$(perm$(t,i,j),k,1) THEN GOSUB 1600
 1520     j=j-1
 1530     GOTO 1500
 1540     x=x+dx:y=y+dy
 1550   NEXT
 1560 NEXT
 1570 RETURN
 1580
 1590 REM RemoveLine
 1600 IF j<permutations(t,i) THEN FOR tmp=j+1 TO permutations(t,i):perm$(t,i,tmp-1)=perm$(t,i,tmp):NEXT
 1610 permutations(t,i)=permutations(t,i)-1
 1620 RETURN
 1630
 1640 REM GetGapPermutations
 1650 gCount=n%(t,i)+1
 1660 FOR j=1 TO gCount:gap(j)=0:NEXT
 1670 gapSize=totalGap
 1680 IF gCount>1 THEN FOR j=1 TO gCount-1:gapSize=gapSize-l%(t,i,j):NEXT
 1690 IF gCount>2 THEN gapSize=gapSize-(gCount-2)
 1700 gapTally=0
 1710 permutationCount=0
 1720 REPEAT
 1730   gap(gCount)=gapSize-gapTally
 1740   GOSUB 1870
 1750   j=0
 1760   REPEAT
 1770     j=j+1
 1780     gap(j)=gap(j)+1
 1790     gapTally=gapTally+1
 1800     IF gapTally>gapSize THEN gapTally=gapTally-gap(j):gap(j)=0
 1810   UNTIL j=gCount OR gap(j)>0
 1820 UNTIL j=gCount
 1830 permutations(t,i)=permutationCount
 1840 RETURN
 1850
 1860 REM AddPermutation
 1870 permutationCount=permutationCount+1
 1880 t$=""
 1890 byte$="1":num=gap(1):GOSUB 1970
 1900 IF gCount>1 byte$="2":num=l%(t,i,1):GOSUB 1970
 1910 IF gCount>2 FOR tmp=2 TO gCount-1:byte$="1":num=gap(tmp)+1:GOSUB 1970:byte$="2":num=l%(t,i,tmp):GOSUB 1970:NEXT
 1920 byte$="1":num=gap(gCount):GOSUB 1970
 1930 perm$(t,i,permutationCount)=t$
 1940 RETURN
 1950
 1960 REM RepeatByte(byte$,num)
 1970 IF num>0 THEN FOR temp=1 TO num:t$=t$+byte$:NEXT
 1980 RETURN
