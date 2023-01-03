      HIMEM=LOMEM+&800000
      PRINT "CREATE AND TEST RANDOM NONOGROM (Y/N)";: INPUT A$
      IF A$ = "Y" THEN
        W% = 20: H% = 20: PROCprepareData
        TIME = 0
        REPEAT
          PROCcreate
          PROCsolve
        UNTIL solveCount=0
        time = TIME
        PROCtransfer: PROCdisplay
        PRINT time/100;" seconds"
        END
      ENDIF
      PRINT "CREATE AND TEST WIKI EXAMPLE (Y/N)";: INPUT A$
      IF A$ = "Y" THEN PROCdemoNonogram: PROCsolve: PROCtransfer: PROCdisplay: END
      END

      REM W% - gridWidth
      REM H% - gridHeight
      REM T% - temp var

      REM g%() - grid
      REM r%() - runLines
      REM n%() - numLines
      REM p%() - gap
      REM m%() - permutations

      REM p$() - perm$

      DEF PROCprepareData
      REM Maximum dimension (width or height)
      S% = W%: IF H% > W% THEN S% = H%
      REM All cells in grid(,) can be 0 (unknown), 1 (empty) or 2 (full)
      DIM g%(W% + 1, H% + 1)
      REM All nonogram line data runLines(a,b,c) where a=1 or 2 (row or column), b=row/column index and c=nonogram run data
      DIM r%(2, S%, (S% + 1) / 2)
      REM Relates to r%(a,b,c) and stores number of entries for c
      DIM n%(2, S%)
      REM Stores all possible gap permutations for a row or column
      DIM p%((S% + 1) / 2 + 1)
      REM Stores all possible m% for each row and column
      DIM p$(2, S%, 6000): REM This will blow up if any row or column has more than 6000 m%
      REM Permutation count for each row and column
      DIM m%(2, S%)
      REM Grid for solving in
      DIM solvedGrid(W%, H%)
      ENDPROC

      DEF PROCdemoNonogram
      W% = 8: H% = 11: PROCprepareData
      REM Set up rows
      n%(1, 1) = 0
      n%(1, 2) = 1: r%(1, 2, 1) = 4
      n%(1, 3) = 1: r%(1, 3, 1) = 6
      n%(1, 4) = 2: r%(1, 4, 1) = 2: r%(1, 4, 2) = 2
      n%(1, 5) = 2: r%(1, 5, 1) = 2: r%(1, 5, 2) = 2
      n%(1, 6) = 1: r%(1, 6, 1) = 6
      n%(1, 7) = 1: r%(1, 7, 1) = 4
      n%(1, 8) = 1: r%(1, 8, 1) = 2
      n%(1, 9) = 1: r%(1, 9, 1) = 2
      n%(1, 10) = 1: r%(1, 10, 1) = 2
      n%(1, 11) = 0
      REM Set up columns
      n%(2, 1) = 0
      n%(2, 2) = 1: r%(2, 2, 1) = 9
      n%(2, 3) = 1: r%(2, 3, 1) = 9
      n%(2, 4) = 2: r%(2, 4, 1) = 2: r%(2, 4, 2) = 2
      n%(2, 5) = 2: r%(2, 5, 1) = 2: r%(2, 5, 2) = 2
      n%(2, 6) = 1: r%(2, 6, 1) = 4
      n%(2, 7) = 1: r%(2, 7, 1) = 4
      n%(2, 8) = 0
      ENDPROC

      DEF PROCtransfer
      FOR Y% = 1 TO H%: FOR X% = 1 TO W%: g%(X%, Y%) = solvedGrid(X%, Y%): NEXT: NEXT
      ENDPROC

      DEF PROCcreate
      FOR Y% = 1 TO H%:FOR X% = 1 TO W%:g%(X%, Y%) = RND(2):NEXT:NEXT
      FOR Y% = 1 TO H%: PROCcreateLine(1,Y%): NEXT
      FOR X% = 1 TO W%: PROCcreateLine(2,X%): NEXT
      ENDPROC

      DEF PROCcreateLine(t,i)
      IF t = 1 THEN x = 1: y = i: dx = 1: dy = 0 ELSE x = i: y = 1: dx = 0: dy = 1
      l = 1
  880 IF x > W% OR y > H% OR g%(x, y) <> 1 THEN GOTO 910
      x = x + dx: y = y + dy
      GOTO 880
  910 IF x > W% OR y > H% THEN GOTO 990
      k = 0
  930 IF x > W% OR y > H% OR g%(x, y) <> 2 THEN GOTO 970
      x = x + dx: y = y + dy
      k = k + 1
      GOTO 930
  970 r%(t, i, l) = k
      l = l + 1
  990 IF x <= W% AND y <= H% THEN GOTO 880
      n%(t, i) = l - 1
      ENDPROC

      DEF PROCdisplay
      L% = 0
      FOR X% = 1 TO W%
        IF n%(2, X%) > L% THEN L% = n%(2, X%)
      NEXT
      FOR J% = L% TO 1 STEP -1
        FOR X% = 1 TO W%
          IF n%(2, X%) >= J% THEN PRINT "  "; RIGHT$(STR$(r%(2, X%, n%(2, X%)+1-J%)),1); " "; ELSE PRINT "    ";
        NEXT
        PRINT
      NEXT
      FOR Y% = 1 TO H%
        FOR X% = 1 TO W%
          PRINT "+---";
        NEXT
        PRINT "+"
        FOR X% = 1 TO W%
          IF g%(X%, Y%) = 2 THEN PRINT "| * "; ELSE IF g%(X%, Y%) = 1 THEN PRINT "|   "; ELSE PRINT "| ? ";
        NEXT
        PRINT "|";
        IF n%(1, Y%) > 0 THEN FOR J% = 1 TO n%(1, Y%): PRINT " "; RIGHT$(STR$(r%(1, Y%, J%)),1);: NEXT
        PRINT
      NEXT
      FOR X% = 1 TO W%
        PRINT "+---";
      NEXT
      PRINT "+"
      ENDPROC

      DEF PROCsolve
      FOR Y% = 1 TO H%:FOR X% = 1 TO W%:solvedGrid(X%, Y%) = 0:NEXT:NEXT
      FOR t = 1 TO 2
        IF t = 1 THEN m = H%: totalGap = W% ELSE m = W%: totalGap = H%
        FOR i = 1 TO m
          PROCgetGapPermutations
        NEXT
      NEXT
      solveCount = W% * H%
 1410 PROCscan
      IF solveCount > 0 AND canSolve = 1 THEN GOTO 1410
      ENDPROC

      DEF PROCscan
      canSolve = 0
      t = 1
      m = H%: totalGap = W%
      PROCsetCommonalities
      t = 2
      m = W%: totalGap = H%
      IF canSolve = 0 THEN GOTO 1540
      PROCremoveNonMatchingLines
 1540 PROCsetCommonalities
      t = 1
      m = H%: totalGap = W%
      IF canSolve = 0 THEN GOTO 1590
      PROCremoveNonMatchingLines
 1590 ENDPROC

      DEF PROCsetCommonalities
      FOR i = 1 TO m
        IF t = 1 THEN x = 1: y = i: dx = 1: dy = 0 ELSE x = i: y = 1: dx = 0: dy = 1
        FOR k = 1 TO totalGap
          v$ = STR$(solvedGrid(x, y))
          IF v$ = "0" THEN
            valid = 1
            v$ = MID$(p$(t, i, 1), k, 1)
            j = 2
            WHILE j <= m%(t, i) AND valid = 1
              IF v$ <> MID$(p$(t, i, j), k, 1) THEN valid = 0
              j = j + 1
            ENDWHILE
            IF valid = 1 THEN solvedGrid(x, y) = VAL(v$): canSolve = 1: solveCount = solveCount - 1
          ENDIF
          x = x + dx: y = y + dy
        NEXT
      NEXT
      ENDPROC

      DEF PROCremoveNonMatchingLines
      FOR i = 1 TO m
        IF t = 1 THEN x = 1: y = i: dx = 1: dy = 0 ELSE x = i: y = 1: dx = 0: dy = 1
        FOR k = 1 TO totalGap
          v% = solvedGrid(x, y)
          IF v% > 0 THEN
            j = m%(t, i)
            v$=STR$(v%)
            WHILE j > 0
              IF v$ <> MID$(p$(t, i, j), k, 1) THEN p$(t, i, j)=""
              j = j - 1
            ENDWHILE
          ENDIF
          x = x + dx: y = y + dy
        NEXT
      NEXT
      PROCremoveNullStrings
      ENDPROC

      DEF PROCremoveLine
      IF j < m%(t, i) THEN FOR T% = j + 1 TO m%(t, i): p$(t, i, T% - 1) = p$(t, i, T%): NEXT
      m%(t, i) = m%(t, i) - 1
      ENDPROC

      DEF PROCremoveNullStrings
      FOR i = 1 TO m
        d=0
        FOR j = 1 TO m%(t, i)
          IF p$(t, i, j)="" THEN d=d+1 ELSE IF d>0 THEN p$(t,i,j-d)=p$(t,i,j)
        NEXT
        m%(t,i)=m%(t,i)-d
      NEXT
      ENDPROC

      DEF PROCgetGapPermutations
      gCount = n%(t, i) + 1
      FOR j = 1 TO gCount: p%(j) = 0: NEXT
      gapSize = totalGap
      IF gCount > 1 THEN FOR j = 1 TO gCount - 1: gapSize = gapSize - r%(t, i, j): NEXT
      IF gCount > 2 THEN gapSize = gapSize - (gCount - 2)
      gapTally = 0
      permutationCount = 0
 2230 p%(gCount) = gapSize - gapTally
      PROCaddPermutation
      j = 0
 2260 j = j + 1
      p%(j) = p%(j) + 1
      gapTally = gapTally + 1
      IF gapTally > gapSize THEN gapTally = gapTally - p%(j): p%(j) = 0
      IF j <> gCount AND p%(j) = 0 THEN GOTO 2260
      IF j <> gCount THEN GOTO 2230
      m%(t, i) = permutationCount
      ENDPROC

      DEF PROCaddPermutation
      permutationCount = permutationCount + 1
      t$ = ""
      PROCrepeatByte("1",p%(1))
      IF gCount > 1 THEN PROCrepeatByte("2",r%(t, i, 1))
      IF gCount > 2 THEN FOR tmp = 2 TO gCount - 1: PROCrepeatByte("1",p%(tmp) + 1): PROCrepeatByte("2",r%(t, i, tmp)):NEXT
      PROCrepeatByte("1",p%(gCount))
      p$(t, i, permutationCount) = t$
      ENDPROC

      DEF PROCrepeatByte(byte$,num)
      IF num > 0 THEN FOR temp = 1 TO num: t$ = t$ + byte$: NEXT
      ENDPROC
