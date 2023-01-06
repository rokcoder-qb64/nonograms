SCREEN _NEWIMAGE(1280, 960, 32)
DO: _LIMIT 10: LOOP UNTIL _SCREENEXISTS
_TITLE "Nonograms"
REM _ScreenMove _Middle

CONST WHITE = _RGB32(255, 255, 255)
CONST GREY = _RGB32(127, 127, 127)
CONST GREEN = _RGB32(0, 255, 0)

CONST TRUE = 1
CONST FALSE = 0

CONST UNKNOWN = 0
CONST EMPTY = 1
CONST FULL = 2

CONST ROW = 1
CONST COLUMN = 2

REDIM SHARED targetGrid%(0, 0)
REM All nonogram line data runLines(a,b,c) where a=1 or 2 (row or column), b=row/column index and c=nonogram run data
REDIM SHARED runs%(0, 0, 0)
REM Relates to r%(a,b,c) and stores number of entries for c
REDIM SHARED numRuns%(0, 0)
REM Stores all possible gap permutations for a row or column
REDIM SHARED gaps%(0)
REM Stores all possible m% for each row and column
REDIM SHARED permutations&(0, 0, 0): REM This will blow up if any row or column has more than 6000 m%
REM Permutation count for each row and column
REDIM SHARED numPermutations%(0, 0)
REM Grid for solving in
REDIM SHARED activeGrid%(0, 0)

DIM SHARED gridSize%, solved%, xOffset%, yOffset%

RANDOMIZE TIMER
CLS

gridSize% = 20: CALL prepareData

solved% = FALSE

DO WHILE solved% = FALSE
    CALL create
    solved% = solve%
LOOP

CALL display(xOffset%, yOffset%)

DO
    _LIMIT 30
    CALL updateMouse(xOffset%, yOffset%)
LOOP UNTIL TRUE = FALSE

END

SUB updateMouse (xOffset%, yOffset%)
    STATIC x%, y%
    DO WHILE _MOUSEINPUT
        x% = (_MOUSEX - 16 - xOffset%) / 32
        y% = (_MOUSEY - 16 - yOffset%) / 32
    LOOP
    LOCATE 20, 1
    PRINT ; x%; ", "; y%; "       "
    IF x% > 0 AND x% <= gridSize% AND y% > 0 AND y% <= gridSize% THEN
        LINE (x% * 32 + xOffset%, y% * 32 + yOffset%)-(x% * 32 + 32 + xOffset%, y% * 32 + 32 + yOffset%), GREEN, BF
    END IF
END SUB

SUB prepareData
    REM All cells in grid(,) can be 0 (unknown), 1 (empty) or 2 (full)
    REDIM targetGrid%(gridSize% + 1, gridSize% + 1)
    REM All nonogram line data runLines(a,b,c) where a=1 or 2 (row or column), b=row/column index and c=nonogram run data
    REDIM runs%(2, gridSize%, (gridSize% + 1) / 2)
    REM Relates to r%(a,b,c) and stores number of entries for c
    REDIM numRuns%(2, gridSize%)
    REM Stores all possible gap permutations for a row or column
    REDIM gaps%((gridSize% + 1) / 2 + 1)
    REM Stores all possible m% for each row and column
    REDIM permutations&(2, gridSize%, 6000): REM This will blow up if any row or column has more than 6000 m%
    REM Permutation count for each row and column
    REDIM numPermutations%(2, gridSize%)
    REM Grid for solving in
    REDIM activeGrid%(gridSize%, gridSize%)
END SUB

SUB transfer
    FOR Y% = 1 TO gridSize%
        FOR X% = 1 TO gridSize%
            targetGrid%(X%, Y%) = activeGrid%(X%, Y%)
        NEXT
    NEXT
END SUB

SUB create
    FOR y% = 1 TO gridSize%
        FOR x% = 1 TO gridSize%
            targetGrid%(x%, y%) = INT(RND * 2) + 1
        NEXT
    NEXT
    FOR z% = 1 TO 2
        FOR i% = 1 TO gridSize%
            CALL createLine(i%, z%)
        NEXT
    NEXT
END SUB

SUB createLine (index%, dir%)
    DIM x%, y%, dx%, dy%, runIndex%, length%
    IF dir% = ROW THEN x% = 1: y% = index%: dx% = 1: dy% = 0 ELSE x% = index%: y% = 1: dx% = 0: dy% = 1
    runIndex% = 1
    DO
        DO WHILE x% <= gridSize% AND y% <= gridSize% AND targetGrid%(x%, y%) = 1
            x% = x% + dx%
            y% = y% + dy%
        LOOP
        IF x% <= gridSize% AND y% <= gridSize% THEN
            length% = 0
            DO WHILE x% <= gridSize% AND y% <= gridSize% AND targetGrid%(x%, y%) = 2
                x% = x% + dx%
                y% = y% + dy%
                length% = length% + 1
            LOOP
            runs%(dir%, index%, runIndex%) = length%
            runIndex% = runIndex% + 1
        END IF
    LOOP UNTIL x% > gridSize% OR y% > gridSize%
    numRuns%(dir%, index%) = runIndex% - 1
END SUB

SUB display (xOffset%, yOffset%)
    DIM x%, y%, maxCountX%, maxCountY%, r$, textX%, i%, temp%
    CLS
    REM Determine longest run of integers for a column
    maxCountY% = 0
    FOR x% = 1 TO gridSize%
        IF numRuns%(COLUMN, x%) > maxCountY% THEN maxCountY% = numRuns%(COLUMN, x%)
    NEXT
    REM Determine longest run of integers for a column
    maxCountX% = 0
    FOR y% = 1 TO gridSize%
        IF numRuns%(ROW, y%) > 0 THEN
            temp% = 1
            FOR i% = 1 TO numRuns%(ROW, y%)
                temp% = temp% + LEN(STR$(runs%(ROW, y%, i%)))
            NEXT
            IF temp% > maxCountX% THEN maxCountX% = temp%
        END IF
    NEXT
    REM Display the column run numbers
    FOR x% = 1 TO gridSize%
        FOR y% = 1 TO numRuns%(COLUMN, x%)
            LOCATE y% + maxCountY% + 2 - numRuns%(COLUMN, x%), maxCountX% + x% * 4
            r$ = STR$(runs%(COLUMN, x%, y%))
            r$ = RIGHT$(r$, LEN(r$) - 1)
            IF LEN(r$) < 2 THEN r$ = " " + r$
            PRINT r$
        NEXT
    NEXT
    REM Display the row run numbers
    FOR y% = 1 TO gridSize%
        textX% = maxCountX% + 1
        FOR x% = numRuns%(ROW, y%) TO 1 STEP -1
            LOCATE maxCountY% + y% * 2 + 2, textX%
            r$ = STR$(runs%(ROW, y%, x%))
            r$ = RIGHT$(r$, LEN(r$) - 1)
            PRINT r$
            textX% = textX% - LEN(r$) - 1
        NEXT
    NEXT
    xOffset% = 8 * maxCountX% - 16
    yOffset% = 16 * maxCountY% + 6
    FOR y% = 1 TO gridSize%
        FOR x% = 1 TO gridSize%
            IF activeGrid%(x%, y%) = 2 THEN LINE (x% * 32 + xOffset%, y% * 32 + yOffset%)-(x% * 32 + 32 + xOffset%, y% * 32 + 32 + yOffset%), WHITE, BF
            LINE (x% * 32 + xOffset%, y% * 32 + yOffset%)-(x% * 32 + 32 + xOffset%, y% * 32 + 32 + yOffset%), GREY, B
        NEXT
    NEXT
END SUB

FUNCTION solve%
    DIM x%, y%, dir%, i%, solvable%
    FOR y% = 1 TO gridSize%
        FOR x% = 1 TO gridSize%
            activeGrid%(x%, y%) = UNKNOWN
        NEXT
    NEXT
    FOR dir% = 1 TO 2
        FOR i% = 1 TO gridSize%
            CALL getGapPermutations(i%, dir%)
        NEXT
    NEXT
    unfilledCount% = gridSize% * gridSize%
    DO
        CALL scan(solvable%, unfilledCount%)
    LOOP UNTIL unfilledCount% = 0 OR solvable% = FALSE
    IF unfilledCount% = 0 THEN solve% = TRUE ELSE solve% = FALSE
END FUNCTION

SUB scan (solvable%, unfilledCount%)
    DIM solvableLine%
    solvable% = FALSE
    soilvableLine% = FALSE
    CALL setCommonalities(ROW, soilvableLine%, unfilledCount%)
    IF soilvableLine% = TRUE THEN solvable% = TRUE: CALL removeNonMatchingLines(COLUMN)
    solvableLine% = FALSE
    CALL setCommonalities(COLUMN, soilvableLine%, unfilledCount%)
    IF soilvableLine% = TRUE THEN solvable% = TRUE: CALL removeNonMatchingLines(ROW)
END SUB

SUB setCommonalities (dir%, solvableLine%, unfilledCount%)
    DIM valid%, i%, k%, j%, x%, y%, dx%, dy%, bitValue&, bitMask&
    FOR i% = 1 TO gridSize%
        IF dir% = ROW THEN x% = 1: y% = i%: dx% = 1: dy% = 0 ELSE x% = i%: y% = 1: dx% = 0: dy% = 1
        bitMask& = 1
        FOR k% = 1 TO gridSize%
            IF activeGrid%(x%, y%) = UNKNOWN THEN
                valid% = TRUE
                bitValue& = permutations&(dir%, i%, 1) AND bitMask&
                j% = 2
                DO WHILE j% <= numPermutations%(dir%, i%) AND valid% = TRUE
                    IF (((bitValue& > 0) <> ((permutations&(dir%, i%, j%) AND bitMask&) > 0))) THEN valid% = FALSE
                    j% = j% + 1
                LOOP
                IF valid% = TRUE THEN
                    IF bitValue& > 0 THEN bitValue& = 1
                    activeGrid%(x%, y%) = bitValue& + 1
                    solvableLine% = TRUE
                    unfilledCount% = unfilledCount% - 1
                END IF
            END IF
            bitMask& = bitMask& * 2
            x% = x% + dx%: y% = y% + dy%
        NEXT
    NEXT
END SUB

SUB removeNonMatchingLines (dir%)
    DIM i%, k%, j%, x%, y%, dx%, dy%, bitValue%, bitMask&
    FOR i% = 1 TO gridSize%
        IF dir% = ROW THEN x% = 1: y% = i%: dx% = 1: dy% = 0 ELSE x% = i%: y% = 1: dx% = 0: dy% = 1
        bitMask& = 1
        FOR k% = 1 TO gridSize%
            bitValue% = activeGrid%(x%, y%)
            IF bitValue% > 0 THEN
                j% = numPermutations%(dir%, i%)
                DO WHILE j% > 0
                    IF (((bitValue% > 1) <> ((permutations&(dir%, i%, j%) AND bitMask&) > 0))) THEN permutations&(dir%, i%, j%) = -1
                    j% = j% - 1
                LOOP
            END IF
            bitMask& = bitMask& * 2
            x% = x% + dx%: y% = y% + dy%
        NEXT
    NEXT
    CALL removeNullStrings(dir%)
END SUB

SUB removeNullStrings (dir%)
    DIM i%, k%, j%
    FOR i% = 1 TO gridSize%
        k% = 0
        FOR j% = 1 TO numPermutations%(dir%, i%)
            IF permutations&(dir%, i%, j%) = -1 THEN k% = k% + 1 ELSE IF k% > 0 THEN permutations&(dir%, i%, j% - k%) = permutations&(dir%, i%, j%)
        NEXT
        numPermutations%(dir%, i%) = numPermutations%(dir%, i%) - k%
    NEXT
END SUB

SUB getGapPermutations (i%, dir%)
    DIM gapCount%, j%, k%, l%, p%
    gapCount% = numRuns%(dir%, i%) + 1
    FOR j% = 1 TO gapCount%
        gaps%(j%) = 0
    NEXT
    k% = gridSize%
    IF gapCount% > 1 THEN FOR j% = 1 TO gapCount% - 1: k% = k% - runs%(dir%, i%, j%): NEXT
    IF gapCount% > 2 THEN k% = k% - (gapCount% - 2)
    l% = 0
    p% = 0
    DO
        gaps%(gapCount%) = k% - l%
        CALL addPermutation(gapCount%, p%, i%, dir%)
        j% = 0
        DO
            j% = j% + 1
            gaps%(j%) = gaps%(j%) + 1
            l% = l% + 1
            IF l% > k% THEN l% = l% - gaps%(j%): gaps%(j%) = 0
        LOOP UNTIL j% = gapCount% OR gaps%(j%) <> 0
    LOOP UNTIL j% = gapCount%
    numPermutations%(dir%, i%) = p%
END SUB

SUB addPermutation (gapCount%, p%, i%, dir%)
    DIM e&, b&, w%
    p% = p% + 1
    e& = 0: b& = 1
    CALL repeatByte(0, gaps%(1), b&, e&)
    IF gapCount% > 1 THEN CALL repeatByte(1, runs%(dir%, i%, 1), b&, e&)
    IF gapCount% > 2 THEN FOR w% = 2 TO gapCount% - 1: CALL repeatByte(0, gaps%(w%) + 1, b&, e&): CALL repeatByte(1, runs%(dir%, i%, w%), b&, e&): NEXT
    CALL repeatByte(0, gaps%(gapCount%), b&, e&)
    permutations&(dir%, i%, p%) = e&
END SUB

SUB repeatByte (byte%, num%, b&, e&)
    DIM t%
    IF num% > 0 THEN
        IF byte% = 0 THEN
            b& = b& * (2 ^ num%)
        ELSE
            FOR t% = 1 TO num%
                e& = e& + 1 * b&: b& = b& * 2
            NEXT
        END IF
    END IF
END SUB
