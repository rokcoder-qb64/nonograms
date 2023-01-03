   10 PRINT "CREATE AND TEST RANDOM NONOGROM (Y/N)";: INPUT A$
   20 IF A$ = "Y" THEN gridWidth = 5: gridHeight = 5: GOSUB 80: GOSUB 560: GOSUB 1100: GOSUB 520: GOSUB 800: END
   30 PRINT "CREATE AND TEST WIKI EXAMPLE (Y/N)";: INPUT A$
   40 IF A$ = "Y" THEN GOSUB 270: GOSUB 1100: GOSUB 520: GOSUB 800: END
   50 END
   60
   70 REM PrepareData
   80 REM Maximum dimension (width or height)
   90 S% = gridWidth: IF gridHeight > gridWidth THEN S% = gridHeight
  100 REM All cells in grid(,) can be 0 (unknown), 1 (empty) or 2 (full)
  110 DIM grid(gridWidth + 1, gridHeight + 1)
  120 REM All nonogram line data runLines(a,b,c) where a=1 or 2 (row or column), b=row/column index and c=nonogram run data
  130 DIM runLines(2, S%, (S% + 1) / 2)
  140 REM Relates to runLines(a,b,c) and stores number of entries for c
  150 DIM numLines(2, S%)
  160 REM Stores all possible gap permutations for a row or column
  170 DIM gap((S% + 1) / 2 + 1)
  180 REM Stores all possible permutations for each row and column
  190 DIM perm$(2, S%, 50): REM This will blow up if any row or column has more than 100 permutations
  200 REM Permutation count for each row and column
  210 DIM permutations(2, S%)
  220 REM Grid for solving in
  230 DIM solvedGrid(gridWidth, gridHeight)
  240 RETURN
  250
  260 REM Set up a "P" nonogram (from the wiki page - https://en.wikipedia.org/wiki/Nonogram#Example)
  270 gridWidth = 8: gridHeight = 11: GOSUB 80
  280 REM Set up rows
  290 numLines(1, 1) = 0
  300 numLines(1, 2) = 1: runLines(1, 2, 1) = 4
  310 numLines(1, 3) = 1: runLines(1, 3, 1) = 6
  320 numLines(1, 4) = 2: runLines(1, 4, 1) = 2: runLines(1, 4, 2) = 2
  330 numLines(1, 5) = 2: runLines(1, 5, 1) = 2: runLines(1, 5, 2) = 2
  340 numLines(1, 6) = 1: runLines(1, 6, 1) = 6
  350 numLines(1, 7) = 1: runLines(1, 7, 1) = 4
  360 numLines(1, 8) = 1: runLines(1, 8, 1) = 2
  370 numLines(1, 9) = 1: runLines(1, 9, 1) = 2
  380 numLines(1, 10) = 1: runLines(1, 10, 1) = 2
  390 numLines(1, 11) = 0
  400 REM Set up columns
  410 numLines(2, 1) = 0
  420 numLines(2, 2) = 1: runLines(2, 2, 1) = 9
  430 numLines(2, 3) = 1: runLines(2, 3, 1) = 9
  440 numLines(2, 4) = 2: runLines(2, 4, 1) = 2: runLines(2, 4, 2) = 2
  450 numLines(2, 5) = 2: runLines(2, 5, 1) = 2: runLines(2, 5, 2) = 2
  460 numLines(2, 6) = 1: runLines(2, 6, 1) = 4
  470 numLines(2, 7) = 1: runLines(2, 7, 1) = 4
  480 numLines(2, 8) = 0
  490 RETURN
  500
  510 REM Transfer
  520 FOR Y% = 1 TO gridHeight: FOR X% = 1 TO gridWidth: grid(X%, Y%) = solvedGrid(X%, Y%): NEXT: NEXT
  530 RETURN
  540
  550 REM Create
  560 FOR Y% = 1 TO gridHeight:FOR X% = 1 TO gridWidth:grid(X%, Y%) = RND(2)MOD 2+1:NEXT:NEXT
  570 FOR Y% = 1 TO gridHeight: t = 1: i = Y%: GOSUB 620: NEXT
  580 FOR X% = 1 TO gridWidth: t = 2: i = X%: GOSUB 620: NEXT
  590 RETURN
  600
  610 REM CreateLine(t,i)
  620 IF t = 1 THEN x = 1: y = i: dx = 1: dy = 0 ELSE x = i: y = 1: dx = 0: dy = 1
  630 l = 1
  640 IF x > gridWidth OR y > gridHeight OR grid(x, y) <> 1 THEN GOTO 670
  650 x = x + dx: y = y + dy
  660 GOTO 640
  670 IF x > gridWidth OR y > gridHeight THEN GOTO 750
  680 k = 0
  690 IF x > gridWidth OR y > gridHeight OR grid(x, y) <> 2 THEN GOTO 730
  700 x = x + dx: y = y + dy
  710 k = k + 1
  720 GOTO 690
  730 runLines(t, i, l) = k
  740 l = l + 1
  750 IF x <= gridWidth AND y <= gridHeight THEN GOTO 640
  760 numLines(t, i) = l - 1
  770 RETURN
  780
  790 REM Display
  800 L% = 0
  810 FOR X% = 1 TO gridWidth
  820   IF numLines(2, X%) > L% THEN L% = numLines(2, X%)
  830 NEXT
  840 FOR J% = L% TO 1 STEP -1
  850   FOR X% = 1 TO gridWidth
  860     IF numLines(2, X%) >= J% THEN PRINT "  "; RIGHT$(STR$(runLines(2, X%, numLines(2, X%)+1-J%)),1); " "; ELSE PRINT "    ";
  870   NEXT
  880   PRINT
  890 NEXT
  900 FOR Y% = 1 TO gridHeight
  910   FOR X% = 1 TO gridWidth
  920     PRINT "+---";
  930   NEXT
  940   PRINT "+"
  950   FOR X% = 1 TO gridWidth
  960     IF grid(X%, Y%) = 2 THEN PRINT "| * "; ELSE IF grid(X%, Y%) = 1 THEN PRINT "|   "; ELSE PRINT "| ? ";
  970   NEXT
  980   PRINT "|";
  990   IF numLines(1, Y%) > 0 THEN FOR J% = 1 TO numLines(1, Y%): PRINT " "; RIGHT$(STR$(runLines(1, Y%, J%)),1);: NEXT
 1000   PRINT
 1010 NEXT
 1020 FOR X% = 1 TO gridWidth
 1030   PRINT "+---";
 1040 NEXT
 1050 PRINT "+"
 1060 RETURN
 1070
 1080 REM Solve
 1090 FOR Y% = 1 TO gridHeight:FOR X% = 1 TO gridWidth:solvedGrid(X%, Y%) = 0:NEXT:NEXT
 1100 FOR t = 1 TO 2
 1110   IF t = 1 THEN m = gridHeight: totalGap = gridWidth ELSE m = gridWidth: totalGap = gridHeight
 1120   FOR i = 1 TO m
 1130     PRINT "Getting permutations for "; t; ","; i
 1140     GOSUB 1850
 1150   NEXT
 1160 NEXT
 1170 solveCount = gridWidth * gridHeight
 1180 GOSUB 1240
 1190 IF solveCount > 0 AND canSolve = 1 THEN GOTO 1180
 1200 PRINT
 1210 RETURN
 1220
 1230 REM Scan
 1240 canSolve = 0
 1250 t = 1
 1260 m = gridHeight: totalGap = gridWidth
 1270 PRINT "Setting commonalities for rows"
 1280 GOSUB 1440
 1290 t = 2
 1300 m = gridWidth: totalGap = gridHeight
 1310 IF canSolve = 0 THEN GOTO 1340
 1320 PRINT "Removing non-matching runLines for columns"
 1330 GOSUB 1640
 1340 PRINT "Setting commonalities for columns"
 1350 GOSUB 1440
 1360 t = 1
 1370 m = gridHeight: totalGap = gridWidth
 1380 IF canSolve = 0 THEN GOTO 1410
 1390 PRINT "Removing non-matching runLines for rows"
 1400 GOSUB 1640
 1410 RETURN
 1420
 1430 REM SetCommonalities
 1440 FOR i = 1 TO m
 1450   IF t = 1 THEN x = 1: y = i: dx = 1: dy = 0 ELSE x = i: y = 1: dx = 0: dy = 1
 1460   FOR k = 1 TO totalGap
 1470     v$ = RIGHT$(STR$(solvedGrid(x, y)),1)
 1480     IF v$ <> "0" THEN GOTO 1580
 1490     valid = 1
 1500     v$ = MID$(perm$(t, i, 1), k, 1)
 1510     IF permutations(t, i) < 2 THEN GOTO 1570
 1520     j = 2
 1530     IF j > permutations(t, i) OR valid <> 1 THEN GOTO 1570
 1540     IF v$ <> MID$(perm$(t, i, j), k, 1) THEN valid = 0
 1550     j = j + 1
 1560     GOTO 1530
 1570     IF valid = 1 THEN solvedGrid(x, y) = VAL(v$): canSolve = 1: solveCount = solveCount - 1
 1580     x = x + dx: y = y + dy
 1590   NEXT
 1600 NEXT
 1610 RETURN
 1620
 1630 REM RemoveNonMatchingLines
 1640 FOR i = 1 TO m
 1650   IF t = 1 THEN x = 1: y = i: dx = 1: dy = 0 ELSE x = i: y = 1: dx = 0: dy = 1
 1660   FOR k = 1 TO totalGap
 1670     v$ = RIGHT$(STR$(solvedGrid(x, y)),1)
 1680     IF v$ = "0" THEN GOTO 1740
 1690     j = permutations(t, i)
 1700     IF j < 1 THEN GOTO 1740
 1710     IF v$ <> MID$(perm$(t, i, j), k, 1) THEN GOSUB 1800
 1720     j = j - 1
 1730     GOTO 1700
 1740     x = x + dx: y = y + dy
 1750   NEXT
 1760 NEXT
 1770 RETURN
 1780
 1790 REM RemoveLine
 1800 IF j < permutations(t, i) THEN FOR tmp = j + 1 TO permutations(t, i): perm$(t, i, tmp - 1) = perm$(t, i, tmp): NEXT
 1810 permutations(t, i) = permutations(t, i) - 1
 1820 RETURN
 1830
 1840 REM GetGapPermutations
 1850 gCount = numLines(t, i) + 1
 1860 FOR j = 1 TO gCount: gap(j) = 0: NEXT
 1870 gapSize = totalGap
 1880 IF gCount > 1 THEN FOR j = 1 TO gCount - 1: gapSize = gapSize - runLines(t, i, j): NEXT
 1890 IF gCount > 2 THEN gapSize = gapSize - (gCount - 2)
 1900 gapTally = 0
 1910 permutationCount = 0
 1920 gap(gCount) = gapSize - gapTally
 1930 GOSUB 2050
 1940 j = 0
 1950 j = j + 1
 1960 gap(j) = gap(j) + 1
 1970 gapTally = gapTally + 1
 1980 IF gapTally > gapSize THEN gapTally = gapTally - gap(j): gap(j) = 0
 1990 IF j <> gCount AND gap(j) = 0 THEN GOTO 1950
 2000 IF j <> gCount THEN GOTO 1920
 2010 permutations(t, i) = permutationCount
 2020 RETURN
 2030
 2040 REM AddPermutation
 2050 permutationCount = permutationCount + 1
 2060 t$ = ""
 2070 byte$ = "1": num = gap(1): GOSUB 2150
 2080 IF gCount > 1 THEN byte$ = "2": num = runLines(t, i, 1): GOSUB 2150
 2090 IF gCount > 2 THEN FOR tmp = 2 TO gCount - 1: byte$ = "1": num = gap(tmp) + 1: GOSUB 2150: byte$ = "2": num = runLines(t, i, tmp): GOSUB 2150: NEXT
 2100 byte$ = "1": num = gap(gCount): GOSUB 2150
 2110 perm$(t, i, permutationCount) = t$
 2120 RETURN
 2130
 2140 REM RepeatByte(byte$,num)
 2150 IF num > 0 THEN FOR temp = 1 TO num: t$ = t$ + byte$: NEXT
 2160 RETURN
