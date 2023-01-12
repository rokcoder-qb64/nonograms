REM ---------------------------------------------------------------------------------------------------------------------------------
REM Nonograms (aka Hanjie)
REM   Programmed by RokCoder (aka Cliff Davies)
REM
REM RokCoder repository - https://github.com/RokCoder
REM Project repository - https://github.com/rokcoder-qb64/nonograms
REM ---------------------------------------------------------------------------------------------------------------------------------

OPTION _EXPLICIT

SCREEN _NEWIMAGE(1280, 960, 32)
$RESIZE:STRETCH
DO: _LIMIT 10: LOOP UNTIL _SCREENEXISTS
_TITLE "Nonograms"

REM ----- Initialise constants ------------------------------------------------------------------------------------------------------

CONST WHITE = _RGB32(255, 255, 255)
CONST GREY = _RGB32(127, 127, 127)
CONST GREEN = _RGB32(0, 255, 0)
CONST BLACK = _RGB32(0, 0, 0)
CONST DARKGREY = _RGB32(64, 64, 64)

CONST TRUE = 1
CONST FALSE = 0

CONST UNKNOWN = 0
CONST EMPTY = 1
CONST FULL = 2

CONST ROW = 1
CONST COLUMN = 2

TYPE BUTTON
    imageHandle AS LONG
    borderHandle AS LONG
    x AS INTEGER
    y AS INTEGER
    currentAlpha AS INTEGER
    targetAlpha AS INTEGER
    w AS INTEGER
    h AS INTEGER
    group AS INTEGER
    pressed AS INTEGER
    active AS INTEGER
END TYPE

REDIM buttons(0) AS BUTTON

REM ---------------------------------------------------------------------------------------------------------------------------------

REDIM targetGrid%(0, 0)
REM All nonogram line data runLines(a,b,c) where a=1 or 2 (row or column), b=row/column index and c=nonogram run data
REDIM runs%(0, 0, 0)
REM Relates to r%(a,b,c) and stores number of entries for c
REDIM numRuns%(0, 0)
REM Stores all possible gap permutations for a row or column
REDIM gaps%(0)
REM Stores all possible m% for each row and column
REDIM permutations&(0, 0, 0): REM This will blow up if any row or column has more than 6000 m%
REM Permutation count for each row and column
REDIM numPermutations%(0, 0)
REM Grid for solving in
REDIM activeGrid%(0, 0)

DIM gridSize%

REM ---------------------------------------------------------------------------------------------------------------------------------

RANDOMIZE TIMER
CLS

REM ---------------------------------------------------------------------------------------------------------------------------------

DIM xOffset%, yOffset%, complete%, s%, d!

DIM button5x5 AS INTEGER
DIM button10x10 AS INTEGER
DIM button15x15 AS INTEGER
DIM button20x20 AS INTEGER
DIM buttonEasy AS INTEGER
DIM buttonNormal AS INTEGER
DIM buttonHard AS INTEGER
DIM buttonPlay AS INTEGER
DIM buttonContinue AS INTEGER

DIM titlePageImage&, gameImage&, zimmer&, click&, tick&, congrats&
DIM buttonBorderImage&, playButtonBorderImage&

titlePageImage& = _LOADIMAGE("assets/nonograms.png", 32)
buttonBorderImage& = _LOADIMAGE("assets/button border.png", 32)
playButtonBorderImage& = _LOADIMAGE("assets/play border.png", 32)
gameImage& = _LOADIMAGE("assets/game.png", 32)
congrats& = _LOADIMAGE("assets/congrats.png", 32)

zimmer& = _SNDOPEN("assets/hans zimmer - time.ogg")
click& = _SNDOPEN("assets/click.ogg")
tick& = _SNDOPEN("assets/tick.ogg")
_SNDLOOP zimmer&

button5x5 = setButton%(1, "button 5x5.png", buttonBorderImage&, 14, 300)
button10x10 = setButton%(1, "button 10x10.png", buttonBorderImage&, 330, 300)
button15x15 = setButton%(1, "button 15x15.png", buttonBorderImage&, 646, 300)
button20x20 = setButton%(1, "button 20x20.png", buttonBorderImage&, 962, 300)
buttonEasy = setButton%(2, "button easy.png", buttonBorderImage&, 172, 510)
buttonNormal = setButton%(2, "button normal.png", buttonBorderImage&, 488, 510)
buttonHard = setButton%(2, "button hard.png", buttonBorderImage&, 804, 510)
buttonPlay = setButton%(-1, "button play.png", playButtonBorderImage&, 340, 810)
buttonContinue = setButton%(-1, "continue.png", playButtonBorderImage&, 340, 810)

pressButton button10x10
pressButton buttonNormal

DO
    _PUTIMAGE (0, 0), titlePageImage&

    drawButton button5x5
    drawButton button10x10
    drawButton button15x15
    drawButton button20x20
    drawButton buttonEasy
    drawButton buttonNormal
    drawButton buttonHard
    drawButton buttonPlay

    waitForNoButton

    DO: _LIMIT 30
        updateButtons
        _DISPLAY
    LOOP UNTIL buttons(buttonPlay).pressed = TRUE

    IF buttons(button5x5).pressed = TRUE THEN s% = 5 ELSE IF buttons(button10x10).pressed = TRUE THEN s% = 10 ELSE IF buttons(button15x15).pressed = TRUE THEN s% = 15 ELSE s% = 20
    IF buttons(buttonEasy).pressed = TRUE THEN d! = 0.2 ELSE IF buttons(buttonNormal).pressed = TRUE THEN d! = 0.35 ELSE d! = 0.5

    removeButtons

    prepareData s%
    createNonogram d!
    resetGrid
    display xOffset%, yOffset%

    DO: _LIMIT 30
        updateMouse xOffset%, yOffset%
        _DISPLAY
        complete% = checkForCompletion%
    LOOP UNTIL complete% = TRUE

    display xOffset%, yOffset%
    _PUTIMAGE (32, 32), congrats&
    drawButton buttonContinue
    waitForNoButton
    DO: _LIMIT 30
        updateButtons
        _DISPLAY
    LOOP UNTIL buttons(buttonContinue).pressed = TRUE

    removeButtons
LOOP

SUB removeButtons
    SHARED buttons() AS BUTTON
    DIM i%
    FOR i% = 1 TO UBOUND(buttons)
        buttons(i%).active = FALSE
    NEXT
END SUB

FUNCTION setButton% (group%, name$, border&, x%, y%)
    SHARED buttons() AS BUTTON
    REDIM _PRESERVE buttons(UBOUND(buttons) + 1) AS BUTTON
    DIM id%
    id% = UBOUND(buttons)
    buttons(id%).imageHandle = _LOADIMAGE("assets/" + name$, 32)
    buttons(id%).borderHandle = border&
    buttons(id%).x = x%
    buttons(id%).y = y%
    buttons(id%).currentAlpha = 192
    buttons(id%).targetAlpha = 192
    buttons(id%).w = _WIDTH(buttons(id%).imageHandle)
    buttons(id%).h = _HEIGHT(buttons(id%).imageHandle)
    buttons(id%).group = group%
    buttons(id%).pressed = FALSE
    buttons(id%).active% = FALSE
    setButton% = id%
END FUNCTION

SUB freeButton (buttonId%)
    SHARED buttons() AS BUTTON
    _FREEIMAGE (buttons(buttonId%).imageHandle)
END SUB

SUB drawButton (buttonId%)
    SHARED buttons() AS BUTTON
    buttons(buttonId%).active = TRUE
    _PUTIMAGE (buttons(buttonId%).x, buttons(buttonId%).y), buttons(buttonId%).imageHandle
    LINE (buttons(buttonId%).x + 10, buttons(buttonId%).y + 10)-(buttons(buttonId%).x + buttons(buttonId%).w - 10, buttons(buttonId%).y + buttons(buttonId%).h - 10), _RGBA32(0, 0, 0, buttons(buttonId%).currentAlpha), BF
    _PUTIMAGE (buttons(buttonId%).x, buttons(buttonId%).y), buttons(buttonId%).borderHandle
END SUB

SUB waitForNoButton
    DIM pressed%
    pressed% = TRUE
    DO
        DO WHILE _MOUSEINPUT
        LOOP
        pressed% = _MOUSEBUTTON(1)
    LOOP UNTIL pressed% = FALSE
END SUB

SUB updateButtons
    SHARED buttons() AS BUTTON
    SHARED click&
    STATIC mouseX%, mouseY%
    DIM i%, j%, pressed%, deltaSgn%, delta%, d&
    pressed% = FALSE
    DO WHILE _MOUSEINPUT
        d& = _DEVICEINPUT
        IF d& THEN
            IF _BUTTONCHANGE(1) = -1 THEN
                pressed% = TRUE
            END IF
        END IF
    LOOP
    mouseX% = _MOUSEX
    mouseY% = _MOUSEY
    FOR i% = 1 TO UBOUND(buttons)
        IF buttons(i%).active = TRUE THEN
            IF mouseX% > buttons(i%).x AND mouseX% < buttons(i%).x + buttons(i%).w AND mouseY% > buttons(i%).y AND mouseY% < buttons(i%).y + buttons(i%).h THEN
                IF buttons(i%).group = -1 THEN
                    buttons(i%).targetAlpha = 0
                END IF
                IF pressed% = TRUE THEN
                    pressButton i%
                    _SNDPLAY (click&)
                END IF
            ELSEIF buttons(i%).group = -1 THEN
                buttons(i%).targetAlpha = 192
            END IF
        END IF
    NEXT
    FOR i% = 1 TO UBOUND(buttons)
        IF buttons(i%).active = TRUE THEN
            deltaSgn% = SGN(buttons(i%).targetAlpha - buttons(i%).currentAlpha)
            IF deltaSgn% <> 0 THEN
                IF deltaSgn% = 1 THEN delta% = 16 ELSE delta% = 32
                FOR j% = 1 TO delta%
                    buttons(i%).currentAlpha = buttons(i%).currentAlpha + SGN(buttons(i%).targetAlpha - buttons(i%).currentAlpha)
                NEXT
                drawButton i%
            END IF
        END IF
    NEXT
END SUB

SUB pressButton (buttonId%)
    SHARED buttons() AS BUTTON
    DIM i%
    FOR i% = 1 TO UBOUND(buttons)
        IF buttons(i%).group = buttons(buttonId%).group THEN
            IF i% = buttonId% THEN buttons(i%).targetAlpha = 0: buttons(i%).pressed = TRUE ELSE buttons(i%).targetAlpha = 192: buttons(i%).pressed = FALSE
        END IF
    NEXT
END SUB

REM ---------------------------------------------------------------------------------------------------------------------------------

FUNCTION checkForCompletion%
    SHARED gridSize%
    SHARED activeGrid%(), targetGrid%()
    DIM x%, y%
    checkForCompletion% = TRUE
    FOR y% = 1 TO gridSize%
        FOR x% = 1 TO gridSize%
            IF activeGrid%(x%, y%) <> targetGrid%(x%, y%) THEN checkForCompletion% = FALSE
        NEXT
    NEXT
END FUNCTION

SUB updateMouse (xOffset%, yOffset%)
    SHARED gridSize%
    SHARED activeGrid%(), targetGrid%()
    SHARED tick&
    STATIC lastX%, lastY%, buttonState%, x%, y%
    DIM d&

    DO WHILE _MOUSEINPUT
        d& = _DEVICEINPUT
        IF d& THEN
            IF _BUTTONCHANGE(1) = 1 THEN
                buttonState% = 0
            ELSEIF _BUTTONCHANGE(1) = -1 THEN
                IF x% > 0 AND x% <= gridSize% AND y% > 0 AND y% <= gridSize% THEN
                    IF activeGrid%(x%, y%) <> FULL THEN buttonState% = FULL ELSE buttonState% = EMPTY
                END IF
            END IF
        END IF
    LOOP

    x% = (_MOUSEX - 16 - xOffset%) / 32
    y% = (_MOUSEY - 16 - yOffset%) / 32

    IF lastX% > 0 AND lastY% > 0 AND lastX% <= gridSize% AND lastY% <= gridSize% THEN
        drawGridSquare lastX%, lastY%, xOffset%, yOffset%
    END IF

    IF x% > 0 AND x% <= gridSize% AND y% > 0 AND y% <= gridSize% THEN
        IF buttonState% > 0 AND activeGrid%(x%, y%) <> buttonState% THEN activeGrid%(x%, y%) = buttonState%: _SNDPLAY (tick&)
        drawGridCursor x%, y%, xOffset%, yOffset%
    ELSE
        buttonState% = 0
    END IF

    lastX% = x%
    lastY% = y%
END SUB

FUNCTION getGridColour& (x%, y%)
    SHARED activeGrid%()
    IF activeGrid%(x%, y%) = 2 THEN
        getGridColour& = WHITE
    ELSEIF activeGrid%(x%, y%) = 0 THEN
        getGridColour& = DARKGREY
    ELSE
        getGridColour& = BLACK
    END IF
END FUNCTION

SUB drawGridSquare (x%, y%, xOffset%, yOffset%)
    LINE (x% * 32 + xOffset%, y% * 32 + yOffset%)-(x% * 32 + 32 + xOffset%, y% * 32 + 32 + yOffset%), getGridColour&(x%, y%), BF
    LINE (x% * 32 + xOffset%, y% * 32 + yOffset%)-(x% * 32 + 32 + xOffset%, y% * 32 + 32 + yOffset%), GREY, B
END SUB

SUB drawGridCursor (x%, y%, xOffset%, yOffset%)
    LINE (x% * 32 + xOffset%, y% * 32 + yOffset%)-(x% * 32 + 32 + xOffset%, y% * 32 + 32 + yOffset%), GREY, BF
    LINE (x% * 32 + 8 + xOffset%, y% * 32 + 8 + yOffset%)-(x% * 32 - 8 + 32 + xOffset%, y% * 32 - 8 + 32 + yOffset%), getGridColour&(x%, y%), BF
END SUB

SUB prepareData (size%)
    SHARED gridSize%
    SHARED targetGrid%()
    SHARED runs%()
    SHARED numRuns%()
    SHARED gaps%()
    SHARED permutations&()
    SHARED numPermutations%()
    SHARED activeGrid%()
    gridSize% = size%
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

SUB resetGrid
    SHARED gridSize%
    SHARED activeGrid%()
    DIM x%, y%
    FOR y% = 1 TO gridSize%
        FOR x% = 1 TO gridSize%
            activeGrid%(x%, y%) = UNKNOWN
        NEXT
    NEXT
END SUB

SUB createNonogram (d!)
    DIM solved%
    solved% = FALSE
    DO WHILE solved% = FALSE
        create d!
        solved% = solve%
    LOOP
END SUB

SUB create (d!)
    SHARED gridSize%
    SHARED targetGrid%()
    DIM x%, y%, z%, i%
    FOR y% = 1 TO gridSize%
        FOR x% = 1 TO gridSize%
            IF RND < d! THEN targetGrid%(x%, y%) = 1 ELSE targetGrid%(x%, y%) = 2
            REM targetGrid%(x%, y%) = INT(RND * 2) + 1
        NEXT
    NEXT
    FOR z% = 1 TO 2
        FOR i% = 1 TO gridSize%
            createLine i%, z%
        NEXT
    NEXT
END SUB

SUB createLine (index%, dir%)
    SHARED gridSize%
    SHARED targetGrid%()
    SHARED runs%()
    SHARED numRuns%()
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
    SHARED gameImage&
    SHARED gridSize%
    SHARED numRuns%()
    SHARED activeGrid%()
    SHARED runs%()
    DIM x%, y%, maxCountX%, maxCountY%, r$, textX%, i%, temp%, totalWidth%, totalHeight%
    _PUTIMAGE (0, 0), gameImage&
    COLOR WHITE, _RGBA32(0, 0, 0, 0)
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
    REM Let's work out how to centre this in the dislay
    totalWidth% = gridSize% * 32 + maxCountX% * 8
    totalHeight% = gridSize% * 32 + maxCountY% * 16
    xOffset% = 640 - totalWidth% / 2 + 8 * maxCountX% - 24
    yOffset% = 480 - totalHeight% / 2 + 16 * maxCountY% - 48
    xOffset% = xOffset% - xOffset% MOD 8
    yOffset% = yOffset% - yOffset% MOD 16
    REM Display the column run numbers
    FOR x% = 1 TO gridSize%
        FOR y% = 1 TO numRuns%(COLUMN, x%)
            LOCATE yOffset% / 16 + y% - numRuns%(COLUMN, x%) + 2, xOffset% / 8 + x% * 4 + 2
            r$ = STR$(runs%(COLUMN, x%, y%))
            r$ = RIGHT$(r$, LEN(r$) - 1)
            IF LEN(r$) < 2 THEN r$ = " " + r$
            PRINT r$
        NEXT
    NEXT
    REM Display the row run numbers
    FOR y% = 1 TO gridSize%
        textX% = 4
        FOR x% = numRuns%(ROW, y%) TO 1 STEP -1
            r$ = STR$(runs%(ROW, y%, x%))
            r$ = RIGHT$(r$, LEN(r$) - 1)
            textX% = textX% - LEN(r$) - 1
            LOCATE yOffset% / 16 + y% * 2 + 2, xOffset% / 8 + textX%
            PRINT r$
        NEXT
    NEXT
    xOffset% = xOffset% - 3
    yOffset% = yOffset% + 6
    FOR y% = 1 TO gridSize%
        FOR x% = 1 TO gridSize%
            drawGridSquare x%, y%, xOffset%, yOffset%
        NEXT
    NEXT
END SUB

FUNCTION solve%
    SHARED gridSize%
    SHARED activeGrid%()
    DIM x%, y%, dir%, i%, solvable%, unfilledCount%
    FOR y% = 1 TO gridSize%
        FOR x% = 1 TO gridSize%
            activeGrid%(x%, y%) = UNKNOWN
        NEXT
    NEXT
    FOR dir% = 1 TO 2
        FOR i% = 1 TO gridSize%
            getGapPermutations i%, dir%
        NEXT
    NEXT
    unfilledCount% = gridSize% * gridSize%
    DO
        scan solvable%, unfilledCount%
    LOOP UNTIL unfilledCount% = 0 OR solvable% = FALSE
    IF unfilledCount% = 0 THEN solve% = TRUE ELSE solve% = FALSE
END FUNCTION

SUB scan (solvable%, unfilledCount%)
    DIM solvableLine%
    solvable% = FALSE
    solvableLine% = FALSE
    setCommonalities ROW, solvableLine%, unfilledCount%
    IF solvableLine% = TRUE THEN solvable% = TRUE: removeNonMatchingLines COLUMN
    solvableLine% = FALSE
    setCommonalities COLUMN, solvableLine%, unfilledCount%
    IF solvableLine% = TRUE THEN solvable% = TRUE: removeNonMatchingLines ROW
END SUB

SUB setCommonalities (dir%, solvableLine%, unfilledCount%)
    SHARED gridSize%
    SHARED activeGrid%()
    SHARED permutations&()
    SHARED numPermutations%()
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
    SHARED gridSize%
    SHARED activeGrid%()
    SHARED numPermutations%()
    SHARED permutations&()
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
    removeNullStrings (dir%)
END SUB

SUB removeNullStrings (dir%)
    SHARED gridSize%
    SHARED numPermutations%()
    SHARED permutations&()
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
    SHARED numRuns%()
    SHARED gaps%()
    SHARED gridSize%
    SHARED runs%()
    SHARED numPermutations%()
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
        addPermutation gapCount%, p%, i%, dir%
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
    SHARED gaps%()
    SHARED runs%()
    SHARED permutations&()
    DIM e&, b&, w%
    p% = p% + 1
    e& = 0: b& = 1
    repeatByte 0, gaps%(1), b&, e&
    IF gapCount% > 1 THEN repeatByte 1, runs%(dir%, i%, 1), b&, e&
    IF gapCount% > 2 THEN FOR w% = 2 TO gapCount% - 1: repeatByte 0, gaps%(w%) + 1, b&, e&: repeatByte 1, runs%(dir%, i%, w%), b&, e&: NEXT
    repeatByte 0, gaps%(gapCount%), b&, e&
    permutations&(dir%, i%, p%) = e&
END SUB

SUB repeatByte (byte%, num%, b&, e&)
    DIM t%
    IF num% > 0 THEN
        IF byte% = 0 THEN
            b& = b& * (2 ^ num%)
        ELSE
            FOR t% = 1 TO num%: e& = e& + 1 * b&: b& = b& * 2: NEXT
        END IF
    END IF
END SUB
