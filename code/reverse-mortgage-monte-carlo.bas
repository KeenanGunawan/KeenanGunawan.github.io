Attribute VB_Name = "MonteCarloSimulation"
Option Explicit

' Portfolio-ready refactor of the simulation loop used in the original workbook.
' It recalculates one stochastic house-price path and stores that path as values.

Public Sub RunHousePriceMonteCarlo()
    Const SIMULATION_COUNT As Long = 1000
    Const FIRST_PATH_ROW As Long = 2
    Const LAST_PATH_ROW As Long = 1692

    Dim sourceSheet As Worksheet
    Dim resultsSheet As Worksheet
    Dim simulationIndex As Long
    Dim previousCalculation As XlCalculation

    On Error GoTo CleanUp

    Set sourceSheet = ThisWorkbook.Worksheets("One MC simulation result")
    Set resultsSheet = ThisWorkbook.Worksheets("Store the results")

    previousCalculation = Application.Calculation
    Application.ScreenUpdating = False
    Application.EnableEvents = False
    Application.Calculation = xlCalculationManual

    For simulationIndex = 1 To SIMULATION_COUNT
        sourceSheet.Calculate

        resultsSheet.Cells(1, simulationIndex).Value = simulationIndex
        resultsSheet.Range( _
            resultsSheet.Cells(FIRST_PATH_ROW, simulationIndex), _
            resultsSheet.Cells(LAST_PATH_ROW, simulationIndex) _
        ).Value = sourceSheet.Range( _
            "E" & FIRST_PATH_ROW & ":E" & LAST_PATH_ROW _
        ).Value
    Next simulationIndex

CleanUp:
    Application.Calculation = previousCalculation
    Application.EnableEvents = True
    Application.ScreenUpdating = True

    If Err.Number <> 0 Then
        MsgBox "Simulation stopped: " & Err.Description, vbExclamation
    End If
End Sub
