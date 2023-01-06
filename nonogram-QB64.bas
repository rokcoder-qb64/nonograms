Const WHITE = _RGB32(255, 255, 255)
Const GREY = _RGB32(127, 127, 127)

Const TRUE = 1
Const FALSE = 0

Const UNKNOWN = 0
Const EMPTY = 1
Const FULL = 2

Const ROW = 1
Const COLUMN = 2

ReDim Shared targetGrid%(0, 0)
Rem All nonogram line data runLines(a,b,c) where a=1 or 2 (row or column), b=row/column index and c=nonogram run data
ReDim Shared runs%(0, 0, 0)
Rem Relates to r%(a,b,c) and stores number of entries for c
ReDim Shared numRuns%(0, 0)
Rem Stores all possible gap permutations for a row or column
ReDim Shared gaps%(0)
Rem Stores all possible m% for each row and column
ReDim Shared permutations%(0, 0, 0): Rem This will blow up if any row or column has more than 6000 m%
Rem Permutation count for each row and column
ReDim Shared numPermutations%(0, 0)
Rem Grid for solving in
ReDim Shared activeGrid(0, 0)

'Dim Shared B%
'Dim Shared C%
'Dim Shared D%
'Dim Shared E%
'Dim Shared G%
'Dim Shared I%
'Dim Shared J%
'Dim Shared K%
'Dim Shared L%
'Dim Shared M%
'Dim Shared P%
'Dim Shared R%
'Dim Shared S%
'Dim Shared T%
'Dim Shared U%
'Dim Shared V%
'Dim Shared W%
'Dim Shared X%
'Dim Shared Y%
'Dim Shared Z%

Dim Shared gridSize%

Randomize Timer
Screen _NewImage(1280, 960, 32)
Cls

gridSize% = 15: Call prepareData

solved% = FALSE

Do While solved% = FALSE
    Call create
    solved = solve
Loop

Call display: Rem Note that the display function only prints single digits for runs (even if they go into double digits)
End

Sub prepareData
    Rem All cells in grid(,) can be 0 (unknown), 1 (empty) or 2 (full)
    ReDim targetGrid%(gridSize% + 1, gridSize% + 1)
    Rem All nonogram line data runLines(a,b,c) where a=1 or 2 (row or column), b=row/column index and c=nonogram run data
    ReDim runs%(2, gridSize%, (gridSize% + 1) / 2)
    Rem Relates to r%(a,b,c) and stores number of entries for c
    ReDim numRuns%(2, gridSize%)
    Rem Stores all possible gap permutations for a row or column
    ReDim gaps%((gridSize% + 1) / 2 + 1)
    Rem Stores all possible m% for each row and column
    ReDim permutations%(2, gridSize%, 6000): Rem This will blow up if any row or column has more than 6000 m%
    Rem Permutation count for each row and column
    ReDim numPermutations%(2, gridSize%)
    Rem Grid for solving in
    ReDim activeGrid(gridSize%, gridSize%)
End Sub

Sub transfer
    For Y% = 1 To gridSize%
        For X% = 1 To gridSize%
            targetGrid%(X%, Y%) = activeGrid(X%, Y%)
        Next
    Next
End Sub

Sub create
    For y% = 1 To gridSize%
        For x% = 1 To gridSize%
            targetGrid%(x%, y%) = Int(Rnd * 2) + 1
        Next
    Next
    For z% = 1 To 2
        For i% = 1 To gridSize%
            Call createLine(i%, z%)
        Next
    Next
End Sub

Sub createLine (index%, dir%)
    Dim x%, y%, dx%, dy%, runIndex%, length%
    If dir% = ROW Then x% = 1: y% = index%: dx% = 1: dy% = 0 Else x% = index%: y% = 1: dx% = 0: dy% = 1
    runIndex% = 1
    Do
        Do While x% <= gridSize% And y% <= gridSize% And targetGrid%(x%, y%) = 1
            x% = x% + dx
            y% = y% + dy%
        Loop
        If x% <= gridSize% And y% <= gridSize% Then
            length% = 0
            Do While x% <= gridSize% And y% <= gridSize% And targetGrid%(x%, y%) = 2
                x% = x% + dx%
                y% = y% + dy%
                length% = length% + 1
            Loop
            runs%(dir%, index%, runIndex%) = length%
            runIndex% = runIndex% + 1
        End If
    Loop Until x% > gridSize% Or y% > gridSize%
    numRuns%(dir%, index%) = runIndex% - 1
End Sub

Sub display
    Dim x%, y%, maxCount%, r$, textX%, yOffset%
    Cls
    Rem Determine longest run of integers for a column
    maxCount% = 0
    For x% = 1 To gridSize%
        If numRuns%(COLUMN, x%) > maxCount% Then maxCount% = numRuns%(COLUMN, x%)
    Next
    Rem Display the column run numbers
    For x% = 1 To gridSize%
        For y% = 1 To numRuns%(COLUMN, x%)
            Locate y% + maxCount% + 2 - numRuns%(COLUMN, x%), x% * 4 + 2
            r$ = Str$(runs%(COLUMN, x%, y%))
            r$ = Right$(r$, Len(r$) - 1)
            If Len(r$) < 2 Then r$ = " " + r$
            Print r$
        Next
    Next
    Rem Display the row run numbers
    For y% = 1 To gridSize%
        textX% = 1
        For x% = 1 To numRuns%(ROW, y%)
            Locate maxCount% + y% * 2 + 2, gridSize% * 4 + textX% + 5
            r$ = Str$(runs%(ROW, y%, x%))
            r$ = Right$(r$, Len(r$) - 1)
            Print r$
            textX% = textX% + Len(r$) + 1
        Next
    Next
    yOffset% = 16 * maxCount% + 4
    For y% = 1 To gridSize%
        For x% = 1 To gridSize%
            If activeGrid(x%, y%) = 2 Then Line (x% * 32, y% * 32 + yOffset%)-(x% * 32 + 32, y% * 32 + 32 + yOffset%), WHITE, BF
            Line (x% * 32, y% * 32 + yOffset%)-(x% * 32 + 32, y% * 32 + 32 + yOffset%), GREY, B
        Next
    Next
End Sub

Function solve
    Dim x%, y%, dir%, i%, solvable%
    For y% = 1 To gridSize%
        For x% = 1 To gridSize%
            activeGrid(x%, y%) = UNKNOWN
        Next
    Next
    For dir% = 1 To 2
        For i% = 1 To gridSize%
            Call getGapPermutations(i%, dir%)
        Next
    Next
    unfilledCount% = gridSize% * gridSize%
    Do
        Call scan(solvable%, unfilledCount%)
    Loop Until unfilledCount% = 0 Or solvable% = FALSE
    If unfilledCount% = 0 Then solve = TRUE Else solve = FALSE
End Function

Sub scan (solvable%, unfilledCount%)
    Dim solvableLine%
    solvable% = FALSE
    soilvableLine% = FALSE
    Call setCommonalities(ROW, soilvableLine%, unfilledCount%)
    If soilvableLine% = TRUE Then solvable% = TRUE: Call removeNonMatchingLines(COLUMN)
    solvableLine% = FALSE
    Call setCommonalities(COLUMN, soilvableLine%, unfilledCount%)
    If soilvableLine% = TRUE Then solvable% = TRUE: Call removeNonMatchingLines(ROW)
End Sub

Sub setCommonalities (dir%, solvableLine%, unfilledCount%)
    Dim valid%, i%, k%, j%, x%, y%, dx%, dy%, bitValue%, bitMask%
    For i% = 1 To gridSize%
        If dir% = ROW Then x% = 1: y% = i%: dx% = 1: dy% = 0 Else x% = i%: y% = 1: dx% = 0: dy% = 1
        bitMask% = 1
        For k% = 1 To gridSize%
            If activeGrid(x%, y%) = UNKNOWN Then
                valid% = TRUE
                bitValue% = permutations%(dir%, i%, 1) And bitMask%
                j% = 2
                Do While j% <= numPermutations%(dir%, i%) And valid% = TRUE
                    If (((bitValue% > 0) <> ((permutations%(dir%, i%, j%) And bitMask%) > 0))) Then valid% = FALSE
                    j% = j% + 1
                Loop
                If valid% = TRUE Then
                    If bitValue% > 0 Then bitValue% = 1
                    activeGrid(x%, y%) = bitValue% + 1
                    solvableLine% = TRUE
                    unfilledCount% = unfilledCount% - 1
                End If
            End If
            bitMask% = bitMask% * 2
            x% = x% + dx%: y% = y% + dy%
        Next
    Next
End Sub

Sub removeNonMatchingLines (dir%)
    Dim i%, k%, j%, x%, y%, dx%, dy%, bitValue%, bitMask%
    For i% = 1 To gridSize%
        If dir% = ROW Then x% = 1: y% = i%: dx% = 1: dy% = 0 Else x% = i%: y% = 1: dx% = 0: dy% = 1
        bitMask% = 1
        For k% = 1 To gridSize%
            bitValue% = activeGrid(x%, y%)
            If bitValue% > 0 Then
                j% = numPermutations%(dir%, i%)
                Do While j% > 0
                    If (((bitValue% > 1) <> ((permutations%(dir%, i%, j%) And bitMask%) > 0))) Then permutations%(dir%, i%, j%) = -1
                    j% = j% - 1
                Loop
            End If
            bitMask% = bitMask% * 2
            x% = x% + dx%: y% = y% + dy%
        Next
    Next
    Call removeNullStrings(dir%)
End Sub

Sub removeNullStrings (dir%)
    Dim i%, k%, j%
    For i% = 1 To gridSize%
        k% = 0
        For j% = 1 To numPermutations%(dir%, i%)
            If permutations%(dir%, i%, j%) = -1 Then k% = k% + 1 Else If k% > 0 Then permutations%(dir%, i%, j% - k%) = permutations%(dir%, i%, j%)
        Next
        numPermutations%(dir%, i%) = numPermutations%(dir%, i%) - k%
    Next
End Sub

Sub getGapPermutations (i%, dir%)
    Dim gapCount%, j%, k%, l%, p%
    gapCount% = numRuns%(dir%, i%) + 1
    For j% = 1 To gapCount%
        gaps%(j%) = 0
    Next
    k% = gridSize%
    If gapCount% > 1 Then For j% = 1 To gapCount% - 1: k% = k% - runs%(dir%, i%, j%): Next
    If gapCount% > 2 Then k% = k% - (gapCount% - 2)
    l% = 0
    p% = 0
    Do
        gaps%(gapCount%) = k% - l%
        Call addPermutation(gapCount%, p%, l%)
        j% = 0
        Do
            j% = j% + 1
            gaps%(j%) = gaps%(j%) + 1
            l% = l% + 1
            If l% > k% Then l% = l% - gaps%(j%): gaps%(j%) = 0
        Loop Until j% = gapCount% Or gaps%(j%) <> 0
    Loop Until j% = gapCount%
    numPermutations%(dir%, i%) = p%
End Sub

Sub addPermutation (gapCount%, p%, l%)
    Dim e%, b%, w%
    p% = p% + 1
    e% = 0: b% = 1
    Call repeatByte(0, gaps%(1), b%, e%)
    If gapCount% > 1 Then Call repeatByte(1, runs%(dir%, I%, 1), b%, e%)
    If gapCount% > 2 Then For w% = 2 To gapCount% - 1: Call repeatByte(0, gaps%(w%) + 1, b%, e%): Call repeatByte(1, runs%(dir%, I%, w%), b%, e%): Next
    Call repeatByte(0, gaps%(gapCount%), b%, e%)
    permutations%(dir%, I%, p%) = e%
End Sub

Sub repeatByte (byte%, num%, b%, e%)
    Dim t%
    If num% > 0 Then
        If byte% = 0 Then
            b% = b% * (2 ^ num%)
        Else
            For t% = 1 To num%
                e% = e% + 1 * b%: b% = b% * 2
            Next
        End If
    End If
End Sub
