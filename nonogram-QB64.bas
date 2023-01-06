Const WHITE = _RGB32(255, 255, 255)
Const GREY = _RGB32(127, 127, 127)

ReDim Shared g%(0, 0)
Rem All nonogram line data runLines(a,b,c) where a=1 or 2 (row or column), b=row/column index and c=nonogram run data
ReDim Shared r%(0, 0, 0)
Rem Relates to r%(a,b,c) and stores number of entries for c
ReDim Shared n%(0, 0)
Rem Stores all possible gap permutations for a row or column
ReDim Shared p%(0)
Rem Stores all possible m% for each row and column
ReDim Shared a%(0, 0, 0): Rem This will blow up if any row or column has more than 6000 m%
Rem Permutation count for each row and column
ReDim Shared m%(0, 0)
Rem Grid for solving in
ReDim Shared s%(0, 0)

Dim Shared B%
Dim Shared C%
Dim Shared D%
Dim Shared E%
Dim Shared G%
Dim Shared I%
Dim Shared J%
Dim Shared K%
Dim Shared L%
Dim Shared M%
Dim Shared P%
Dim Shared R%
Dim Shared S%
Dim Shared T%
Dim Shared U%
Dim Shared V%
Dim Shared W%
Dim Shared X%
Dim Shared Y%
Dim Shared Z%

Randomize Timer
Screen _NewImage(1280, 960, 32)
Cls

S% = 10: Call prepareData

TIME = Timer
iterations = 0

Print "Creating"
Call create
Print "Creating run lines"
Call createRunLines
Print "Solving"
Call solve

If C% > 0 Then
    Print "Hiccup"
    For Y% = 1 To S%
        For X% = 1 To S%
            g%(X%, Y%) = s%(X%, Y%)
            If g%(X%, Y%) = 0 Then
                g%(X%, Y%) = 2
            End If
        Next
    Next
    Print "Creating run lines"
    Call createRunLines
    Print "Solving"
    Call solve: Rem It shouldn't be necessary to perform this step if this method makes them all solvable
End If

Print "solved"

TIME = Timer - TIME

Call display: Rem Note that the display function only prints single digits for runs (even if they go into double digits)

Print "Created in "; TIME / 100; " seconds"
If C% > 0 Then Print "But not solvable"

End

Rem B% - rolling 2^ bit value
Rem C% - solve counter
Rem D% - boolean of whether row/column is solvable
Rem E% - bit value
Rem G% - gap count
Rem I% - index
Rem J% - index
Rem K% - counter
Rem L% - counter
Rem N% - counter
Rem O% - temp var
Rem P% - permutation counter
Rem R% - valid (boolean)
Rem S% - gridWidth/gridHeight
Rem T% - temp var
Rem U% - delta x
Rem V% - delta y
Rem W% - temp var
Rem X% - column index
Rem Y% - row index
Rem Z% - line type (row=0, column=1)

Rem a%() - all permutations of rows/columns
Rem g%() - grid
Rem m%() - number of permutationsfor each row/column
Rem n%() - numLines
Rem p%() - gap
Rem r%() - runLines
Rem s%() - play grid

Sub prepareData
    Rem All cells in grid(,) can be 0 (unknown), 1 (empty) or 2 (full)
    ReDim g%(S% + 1, S% + 1)
    Rem All nonogram line data runLines(a,b,c) where a=1 or 2 (row or column), b=row/column index and c=nonogram run data
    ReDim r%(2, S%, (S% + 1) / 2)
    Rem Relates to r%(a,b,c) and stores number of entries for c
    ReDim n%(2, S%)
    Rem Stores all possible gap permutations for a row or column
    ReDim p%((S% + 1) / 2 + 1)
    Rem Stores all possible m% for each row and column
    ReDim a%(2, S%, 6000): Rem This will blow up if any row or column has more than 6000 m%
    Rem Permutation count for each row and column
    ReDim m%(2, S%)
    Rem Grid for solving in
    ReDim s%(S%, S%)
End Sub

Sub transfer
    For Y% = 1 To S%
        For X% = 1 To S%
            g%(X%, Y%) = s%(X%, Y%)
        Next
    Next
End Sub

Sub create
    For Y% = 1 To S%
        For X% = 1 To S%
            g%(X%, Y%) = Int(Rnd * 2) + 1
        Next
    Next
End Sub

Sub createRunLines
    For Z% = 1 To 2
        For I% = 1 To S%
            Call createLine(I%, Z%)
        Next
    Next
End Sub

Sub createLine (I%, Z%)
    If Z% = 1 Then X% = 1: Y% = I%: U% = 1: V% = 0 Else X% = I%: Y% = 1: U% = 0: V% = 1
    L% = 1
    Do
        Do While X% <= S% And Y% <= S% And g%(X%, Y%) = 1
            X% = X% + U%: Y% = Y% + V%
        Loop
        If X% <= S% And Y% <= S% Then
            K% = 0
            Do While X% <= S% And Y% <= S% And g%(X%, Y%) = 2
                X% = X% + U%: Y% = Y% + V%
                K% = K% + 1
            Loop
            r%(Z%, I%, L%) = K%
            L% = L% + 1
        End If
    Loop Until X% > S% Or Y% > S%
    n%(Z%, I%) = L% - 1
End Sub

Sub display
    Cls

    Rem Determine longest run of integers for a column
    L% = 0
    For X% = 1 To S%
        If n%(2, X%) > L% Then L% = n%(2, X%)
    Next
    Rem Display the vertically justified numbers
    For J% = L% To 1 Step -1
        For X% = 1 To S%
            If n%(2, X%) >= J% Then Print "  "; Right$(Str$(r%(2, X%, n%(2, X%) + 1 - J%)), 1); " "; Else Print "    ";
        Next
        Print
    Next

    For Y% = 1 To S%
        For X% = 1 To S%
            If s%(X%, Y%) = 2 Then Line (X% * 32, Y% * 32)-(X% * 32 + 32, Y% * 32 + 32), WHITE, BF
            Line (X% * 32, Y% * 32)-(X% * 32 + 32, Y% * 32 + 32), GREY, B
        Next
        Rem         If n%(1, Y%) > 0 Then For J% = 1 To n%(1, Y%): Print " "; Right$(Str$(r%(1, Y%, J%)), 1);: Next
        Rem     Print
    Next

End Sub

Sub solve
    For Y% = 1 To S%
        For X% = 1 To S%
            s%(X%, Y%) = 0
        Next
    Next
    For Z% = 1 To 2
        For I% = 1 To S%
            Call getGapPermutations(I%, Z%)
        Next
    Next
    C% = S% * S%
    Do
        Call scan
    Loop Until C% = 0 Or D% = 0
End Sub

Sub scan
    doable% = 0
    D% = 0
    Call setCommonalities(1)
    If D% = 1 Then doable% = 1: Call removeNonMatchingLines(2)
    D% = 0
    Call setCommonalities(2)
    If D% = 1 Then doable% = 1: Call removeNonMatchingLines(1)
    D% = doable%
End Sub

Sub setCommonalities (Z%)
    For I% = 1 To S%
        If Z% = 1 Then X% = 1: Y% = I%: U% = 1: V% = 0 Else X% = I%: Y% = 1: U% = 0: V% = 1
        B% = 1
        For K% = 1 To S%
            If s%(X%, Y%) = 0 Then
                R% = 1
                E% = a%(Z%, I%, 1) And B%
                J% = 2
                Do While J% <= m%(Z%, I%) And R% = 1
                    If (((E% > 0) <> ((a%(Z%, I%, J%) And B%) > 0))) Then R% = 0
                    J% = J% + 1
                Loop
                If R% = 1 Then
                    If E% > 0 Then E% = 1
                    s%(X%, Y%) = E% + 1
                    D% = 1: C% = C% - 1
                End If
            End If
            B% = B% * 2
            X% = X% + U%: Y% = Y% + V%
        Next
    Next
End Sub

Sub removeNonMatchingLines (Z%)
    For I% = 1 To S%
        If Z% = 1 Then X% = 1: Y% = I%: U% = 1: V% = 0 Else X% = I%: Y% = 1: U% = 0: V% = 1
        B% = 1
        For K% = 1 To S%
            E% = s%(X%, Y%)
            If E% > 0 Then
                J% = m%(Z%, I%)
                Do While J% > 0
                    If (((E% > 1) <> ((a%(Z%, I%, J%) And B%) > 0))) Then a%(Z%, I%, J%) = -1
                    J% = J% - 1
                Loop
            End If
            B% = B% * 2
            X% = X% + U%: Y% = Y% + V%
        Next
    Next
    Call removeNullStrings(Z%)
End Sub

Sub removeNullStrings (Z%)
    For I% = 1 To S%
        K% = 0
        For J% = 1 To m%(Z%, I%)
            If a%(Z%, I%, J%) = -1 Then K% = K% + 1 Else If K% > 0 Then a%(Z%, I%, J% - K%) = a%(Z%, I%, J%)
        Next
        m%(Z%, I%) = m%(Z%, I%) - K%
    Next
End Sub

Sub getGapPermutations (I%, Z%)
    G% = n%(Z%, I%) + 1
    For J% = 1 To G%
        p%(J%) = 0
    Next
    K% = S%
    If G% > 1 Then For J% = 1 To G% - 1: K% = K% - r%(Z%, I%, J%): Next
    If G% > 2 Then K% = K% - (G% - 2)
    L% = 0
    P% = 0
    Do
        p%(G%) = K% - L%
        Call addPermutation
        J% = 0
        Do
            J% = J% + 1
            p%(J%) = p%(J%) + 1
            L% = L% + 1
            If L% > K% Then L% = L% - p%(J%): p%(J%) = 0
        Loop Until J% = G% Or p%(J%) <> 0
    Loop Until J% = G%
    m%(Z%, I%) = P%
End Sub

Sub addPermutation
    P% = P% + 1
    E% = 0: B% = 1
    Call repeatByte(0, p%(1))
    If G% > 1 Then Call repeatByte(1, r%(Z%, I%, 1))
    If G% > 2 Then For W% = 2 To G% - 1: Call repeatByte(0, p%(W%) + 1): Call repeatByte(1, r%(Z%, I%, W%)): Next
    Call repeatByte(0, p%(G%))
    a%(Z%, I%, P%) = E%
End Sub

Sub repeatByte (byte%, num%)
    If num% > 0 Then
        If byte% = 0 Then
            B% = B% * (2 ^ num%)
        Else
            For T% = 1 To num%
                E% = E% + 1 * B%: B% = B% * 2
            Next
        End If
    End If
End Sub
