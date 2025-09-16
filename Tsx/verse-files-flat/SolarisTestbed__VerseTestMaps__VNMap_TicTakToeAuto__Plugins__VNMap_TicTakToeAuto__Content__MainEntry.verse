
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Random }


PlayersFigures:=enum:
    Cross,
    Zero


MainEntry := class(creative_device):

    @editable
    EventsSystem:EventsBus = EventsBus{}

    @editable
    MoveSleepTime:float = 1.0

    @editable
    Pointers:[]creative_prop = array{}

    @editable
    Cross:creative_prop_asset = DefaultCreativePropAsset

    @editable
    Zero:creative_prop_asset = DefaultCreativePropAsset

    @editable
    CrossExample:creative_prop = creative_prop{}

    @editable
    ZeroExample:creative_prop = creative_prop{}

    @editable
    LightExample:customizable_light_device = customizable_light_device{}

    var BoardOfPositions:[][]vector3 = array{}

    var CrossScale:vector3 = vector3{}
    var CrossRotation:rotation = rotation{}
    var CrossY:float = 0.0 

    var ZeroScale:vector3 = vector3{}
    var ZeroRotation:rotation = rotation{}
    var ZeroY:float = 0.0

    var BoardOfResults:[][]int = array{}
    var TurnsList:[]tuple(int, int) = array{}

    var WinnerDefined:logic = false
    var TurnsFinished:logic = false

    var Winner:int = -1


    OnBegin<override>()<suspends>:void=
        Print("Hello, TIC TAK TOE Game!")

        set BoardOfPositions = PreparePositionsBoardByPointers(Pointers)
        InitBasicScaleRotationOfFigures()
        HideHelperObjects()
        set BoardOfResults = InitResultBoard()
        set TurnsList = MakeTurnsList(BoardOfResults)

        Print("Preparations Finished. START GAME in 2..1..")
        Sleep(2.0)
        ProcessGame()



    PreparePositionsBoardByPointers(InputPointers:[]creative_prop):[][]vector3=
        var col:int = 0
        var row:int = 0

        var board:[][]vector3 = 
            for (Row := 0..2):
                for(Column := 0..2):
                    vector3{}

        for(i:=0..InputPointers.Length - 1):
            if (c:int = Mod[i, 3]) 
            then set col = c 
            else Print ("Can't take Mod for Column calculation")
            
            if(Pointer := InputPointers[i]):
                Location:=Pointer.GetTransform().Translation
                if:
                    set board[row][col] = Location
                else:
                    Print("Can't set in board value in cel wit [Row, Col = {row}, {col}]")
            else:
                Print("Can't take Pointer from array with [i = {i}]")

            if (col = 2):
                set row += 1

        return board

    
    PrintBoard(InputBoard:[][]vector3):void=
        if (NumberOfColumns : int = InputBoard[0].Length):
            for(Row := 0..InputBoard.Length-1, Column := 0..NumberOfColumns-1):
                 if (Element := InputBoard[Row][Column]):
                     Print("[{Element}] at index [{Row}][{Column}]")


    PrintBoardResults(InputBoard:[][]int):void=
        if (NumberOfColumns : int = InputBoard[0].Length):
            for(Row := 0..InputBoard.Length-1, Column := 0..NumberOfColumns):
                 if (Element := InputBoard[Row][Column]):
                     Print("[{Element}] at index [{Row}][{Column}]")

            
    Print(InputTurns:[]tuple(int, int)):void=
        for(i:=0..InputTurns.Length):
            if (t:tuple(int, int) = InputTurns[i]) then Print("{t(0)} - {t(1)}")


    InitBasicScaleRotationOfFigures():void=
        set CrossScale = CrossExample.GetTransform().Scale
        set CrossRotation = CrossExample.GetTransform().Rotation
        set CrossY = CrossExample.GetTransform().Translation.Y
        set ZeroScale = ZeroExample.GetTransform().Scale
        set ZeroRotation = ZeroExample.GetTransform().Rotation
        set ZeroY = ZeroExample.GetTransform().Translation.Y


    HideHelperObjects():void=
        CrossExample.Hide()
        ZeroExample.Hide()
        for(i:=0..Pointers.Length - 1):
            if (P:=Pointers[i]):
                P.Hide()


    InitResultBoard():[][]int=
        board:[][]int=
            for(Row:=0..2):
                for(Column:=0..2):
                    -1

    
    CheckWinCondition():int=
        # Cross: 1, Zero: 0, Draw: -1
        var winner:int = -1

        # Check horizontals
        if (w:int = BoardOfResults[0][0]=BoardOfResults[0][1]=BoardOfResults[0][2]) then return w
        if (w:int = BoardOfResults[1][0]=BoardOfResults[1][1]=BoardOfResults[1][2]) then return w
        if (w:int = BoardOfResults[2][0]=BoardOfResults[2][1]=BoardOfResults[2][2]) then return w 

        # Check diagonals
        if (w:int = BoardOfResults[0][0]=BoardOfResults[1][1]=BoardOfResults[2][2]) then return w
        if (w:int = BoardOfResults[0][2]=BoardOfResults[1][1]=BoardOfResults[2][0]) then return w

        # Check verticals
        if (w:int = BoardOfResults[0][0]=BoardOfResults[1][0]=BoardOfResults[2][0]) then return w
        if (w:int = BoardOfResults[0][1]=BoardOfResults[1][1]=BoardOfResults[2][1]) then return w
        if (w:int = BoardOfResults[0][2]=BoardOfResults[1][2]=BoardOfResults[2][2]) then return w

        return winner


    MakeTurnsList(InputArray:[][]int):[]tuple(int, int)=
        var list:[]tuple(int, int) = array{}
        if (NumberOfColumns : int = InputArray[0].Length):
            for(Row := 0..InputArray.Length-1, Column := 0..NumberOfColumns-1):
                set list += array{(Row, Column)}
        return list

    
    ProcessGame()<suspends>:void=
        loop:
            EventsSystem.FigureTurnEvent.Signal(1)
            Sleep(2.0)
            MakeTurn(PlayersFigures.Cross)
            set Winner = CheckWinCondition()
            if (Winner >= 0) then break
            if (not IsTurnsStillExist() = true) then break
            
            EventsSystem.FigureTurnEvent.Signal(0)
            Sleep(2.0)
            MakeTurn(PlayersFigures.Zero)
            set Winner = CheckWinCondition()
            if (Winner >= 0) then break
            if (not IsTurnsStillExist() = true) then break

        Sleep(2.0)
        ProcessResult(Winner)


    ProcessResult(InputWinner:int)<suspends>:void=
        case(InputWinner):
            1 => 
                Print("CROSS PLAYER WINS")
                EventsSystem.GameFinishEvent.Signal(1)
            0 => 
                Print("ZERO PLAYER WINS")
                EventsSystem.GameFinishEvent.Signal(0)
            _ => 
                Print("DRAW")
                EventsSystem.GameFinishEvent.Signal(-1)
        

    IsTurnsStillExist()<transacts>:logic=
        if (TurnsList.Length > 0) then true else false


    MakeTurn(InputFigure:PlayersFigures):void=
        RandTurnNum:int = GetRandomInt(0, TurnsList.Length - 1)
        var Row:int = 0
        var Col:int = 0
        FigurePosition:vector3 = vector3{}

        if (T:tuple(int, int) = TurnsList[RandTurnNum], set Row = T(0), set Col = T(1)):
            if:
                set TurnsList = TurnsList.RemoveElement[RandTurnNum]
            
            if(var V:vector3 = BoardOfPositions[Row][Col]):
                case(InputFigure):
                    PlayersFigures.Cross => 
                                            set V.Y = CrossY
                                            SpawnProp(Cross, transform{Scale:=CrossScale, Rotation:=CrossRotation, Translation:=V})
                                            if:
                                                set BoardOfResults[Row][Col] = 1

                    PlayersFigures.Zero => 
                                            set V.Y = ZeroY
                                            SpawnProp(Zero, transform{Scale:=ZeroScale, Rotation:=ZeroRotation, Translation:=V})
                                            if: 
                                                set BoardOfResults[Row][Col] = 0
            
            
