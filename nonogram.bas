      HIMEM=LOMEM+&800000

      S% = 20: PROCprepareData

      TIME = 0
      REPEAT
        PROCcreate
        PROCsolve
      UNTIL C%=0
      time = TIME

      PROCtransfer: PROCdisplay

      PRINT time/100;" seconds"

      END

      REM B% - rolling 2^ bit value
      REM C% - solve counter
      REM D% - boolean of whether row/column is solvable
      REM E% - bit value
      REM G% - gap count
      REM I% - index
      REM J% - index
      REM K% - counter
      REM L% - counter
      REM N% - counter
      REM O% - temp var
      REM P% - permutation counter
      REM R% - valid (boolean)
      REM S% - gridWidth/gridHeight
      REM T% - temp var
      REM U% - delta x
      REM V% - delta y
      REM W% - temp var
      REM X% - column index
      REM Y% - row index
      REM Z% - line type (row=0, column=1)

      REM a%() - perm$
      REM g%() - grid
      REM m%() - permutations
      REM n%() - numLines
      REM p%() - gap
      REM r%() - runLines
      REM s%() - play grid

      DEF PROCprepareData
      REM All cells in grid(,) can be 0 (unknown), 1 (empty) or 2 (full)
      DIM g%(S% + 1, S% + 1)
      REM All nonogram line data runLines(a,b,c) where a=1 or 2 (row or column), b=row/column index and c=nonogram run data
      DIM r%(2, S%, (S% + 1) / 2)
      REM Relates to r%(a,b,c) and stores number of entries for c
      DIM n%(2, S%)
      REM Stores all possible gap permutations for a row or column
      DIM p%((S% + 1) / 2 + 1)
      REM Stores all possible m% for each row and column
      DIM a%(2, S%, 6000): REM This will blow up if any row or column has more than 6000 m%
      REM Permutation count for each row and column
      DIM m%(2, S%)
      REM Grid for solving in
      DIM s%(S%, S%)
      ENDPROC

      DEF PROCtransfer
      FOR Y% = 1 TO S%: FOR X% = 1 TO S%: g%(X%, Y%) = s%(X%, Y%): NEXT: NEXT
      ENDPROC

      DEF PROCcreate
      FOR Y% = 1 TO S%:FOR X% = 1 TO S%:g%(X%, Y%) = RND(2):NEXT:NEXT
      FOR Z% = 1 TO 2: FOR I% = 1 TO S%: PROCcreateLine: NEXT: NEXT
      ENDPROC

      DEF PROCcreateLine
      IF Z% = 1 THEN X% = 1: Y% = I%: U% = 1: V% = 0 ELSE X% = I%: Y% = 1: U% = 0: V% = 1
      L% = 1
      REPEAT
        WHILE X% <= S% AND Y% <= S% AND g%(X%, Y%) = 1
          X% = X% + U%: Y% = Y% + V%
        ENDWHILE
        IF X% <= S% AND Y% <= S% THEN
          K% = 0
          WHILE X% <= S% AND Y% <= S% AND g%(X%, Y%) = 2
            X% = X% + U%: Y% = Y% + V%
            K% = K% + 1
          ENDWHILE
          r%(Z%, I%, L%) = K%
          L% = L% + 1
        ENDIF
      UNTIL X% > S% OR Y% > S%
      n%(Z%, I%) = L% - 1
      ENDPROC

      DEF PROCdisplay
      L% = 0
      FOR X% = 1 TO S%
        IF n%(2, X%) > L% THEN L% = n%(2, X%)
      NEXT
      FOR J% = L% TO 1 STEP -1
        FOR X% = 1 TO S%
          IF n%(2, X%) >= J% THEN PRINT "  "; RIGHT$(STR$(r%(2, X%, n%(2, X%)+1-J%)),1); " "; ELSE PRINT "    ";
        NEXT
        PRINT
      NEXT
      FOR Y% = 1 TO S%
        FOR X% = 1 TO S%
          PRINT "+---";
        NEXT
        PRINT "+"
        FOR X% = 1 TO S%
          IF g%(X%, Y%) = 2 THEN PRINT "| * "; ELSE IF g%(X%, Y%) = 1 THEN PRINT "|   "; ELSE PRINT "| ? ";
        NEXT
        PRINT "|";
        IF n%(1, Y%) > 0 THEN FOR J% = 1 TO n%(1, Y%): PRINT " "; RIGHT$(STR$(r%(1, Y%, J%)),1);: NEXT
        PRINT
      NEXT
      FOR X% = 1 TO S%
        PRINT "+---";
      NEXT
      PRINT "+"
      ENDPROC

      DEF PROCsolve
      FOR Y% = 1 TO S%:FOR X% = 1 TO S%:s%(X%, Y%) = 0:NEXT:NEXT
      FOR Z% = 1 TO 2
        FOR I% = 1 TO S%
          PROCgetGapPermutations
        NEXT
      NEXT
      C% = S% * S%
      REPEAT
        PROCscan
      UNTIL C% = 0 OR D% = 0
      ENDPROC

      DEF PROCscan
      D% = 0
      Z% = 1
      PROCsetCommonalities
      Z% = 2
      IF D% = 1 THEN PROCremoveNonMatchingLines
      PROCsetCommonalities
      Z% = 1
      IF D% = 1 THEN PROCremoveNonMatchingLines
      ENDPROC

      DEF PROCsetCommonalities
      FOR I% = 1 TO S%
        IF Z% = 1 THEN X% = 1: Y% = I%: U% = 1: V% = 0 ELSE X% = I%: Y% = 1: U% = 0: V% = 1
        B% = 1
        FOR K% = 1 TO S%
          IF s%(X%, Y%) = 0 THEN
            R% = 1
            E% = a%(Z%,I%,1) AND B%
            J% = 2
            WHILE J% <= m%(Z%, I%) AND R% = 1
              IF (((E%>0)<>((a%(Z%,I%,J%) AND B%)>0))) THEN R% = 0
              J% = J% + 1
            ENDWHILE
            IF R% = 1 THEN
              IF E%>0 THEN E%=1
              s%(X%, Y%) = E%+1
              D% = 1: C% = C% - 1
            ENDIF
          ENDIF
          B% = B% * 2
          X% = X% + U%: Y% = Y% + V%
        NEXT
      NEXT
      ENDPROC

      DEF PROCremoveNonMatchingLines
      FOR I% = 1 TO S%
        IF Z% = 1 THEN X% = 1: Y% = I%: U% = 1: V% = 0 ELSE X% = I%: Y% = 1: U% = 0: V% = 1
        B% = 1
        FOR K% = 1 TO S%
          E% = s%(X%, Y%)
          IF E% > 0 THEN
            J% = m%(Z%, I%)
            WHILE J% > 0 AND a%(Z%, I%, J%)<>-1
              IF (((E%>1)<>((a%(Z%,I%,J%) AND B%)>0))) THEN a%(Z%, I%, J%)=-1
              J% = J% - 1
            ENDWHILE
          ENDIF
          B%=B%*2
          X% = X% + U%: Y% = Y% + V%
        NEXT
      NEXT
      PROCremoveNullStrings
      ENDPROC

      DEF PROCremoveNullStrings
      FOR I% = 1 TO S%
        K%=0
        FOR J% = 1 TO m%(Z%, I%)
          IF a%(Z%, I%, J%)=-1 THEN K%=K%+1 ELSE IF K%>0 THEN a%(Z%,I%,J%-K%)=a%(Z%,I%,J%)
        NEXT
        m%(Z%,I%)=m%(Z%,I%)-K%
      NEXT
      ENDPROC

      DEF PROCgetGapPermutations
      G% = n%(Z%, I%) + 1
      FOR J% = 1 TO G%: p%(J%) = 0: NEXT
      K% = S%
      IF G% > 1 THEN FOR J% = 1 TO G% - 1: K% = K% - r%(Z%, I%, J%): NEXT
      IF G% > 2 THEN K% = K% - (G% - 2)
      L% = 0
      P% = 0
      REPEAT
        p%(G%) = K% - L%
        PROCaddPermutation
        J% = 0
        REPEAT
          J% = J% + 1
          p%(J%) = p%(J%) + 1
          L% = L% + 1
          IF L% > K% THEN L% = L% - p%(J%): p%(J%) = 0
        UNTIL J% = G% OR p%(J%) <> 0
      UNTIL J% = G%
      m%(Z%, I%) = P%
      ENDPROC

      DEF PROCaddPermutation
      P% = P% + 1
      E% = 0:B% = 1
      N% = p%(1): PROCrepeatByte
      IF G% > 1 THEN N% = r%(Z%, I%, 1): O% = 1: PROCrepeatByte
      IF G% > 2 THEN FOR W% = 2 TO G% - 1: N% = p%(W%) + 1: O% = 0: PROCrepeatByte: N% = r%(Z%, I%, W%): O% = 1: PROCrepeatByte:NEXT
      N% = p%(G%): O% = 0: PROCrepeatByte
      a%(Z%, I%, P%) = E%
      ENDPROC

      DEF PROCrepeatByte
      IF N% > 0 THEN
        IF O% = 0 THEN
          B% = B% * (2 ^ N%)
        ELSE
          FOR T% = 1 TO N%
            E% = E% + 1 * B%:B% = B% * 2
          NEXT
        ENDIF
      ENDIF
      ENDPROC
