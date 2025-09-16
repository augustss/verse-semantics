
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Random }
using { GameEvents }
using { Utils }


TornadosController := class(creative_device):

    @editable
    GameplayEvents:EventsBus = EventsBus{}

    @editable
    Tornados:[]vfx_spawner_device = array{}

    @editable
    Points:[]creative_prop = array{}

    @editable
    MinTime:float = 30.0

    @editable
    MaxTime:float = 60.0


    var StartPosition:vector3 = vector3{}
    var EndPosition:vector3 = vector3{}

    var MoveResults:[]move_to_result = array{}

    var Z:float = 0.0



    OnBegin<override>()<suspends>:void=
        Print("Tornados Controller Starts")
        InitializeZAxisVal()
        InitializeZoneTransforms()
        spawn{MoveResultChecker()}
        StartMovingAllTornados()


    
    StartMovingAllTornados():void=
        Print("Start Moving All tronados")
        for(i:=0..Tornados.Length-1):
            Print("Calculate new destination and time for i [{i}] tronado")
            newVector:vector3 = vector3{X:=GetRandomFloat(StartPosition.X, EndPosition.X), Y:=GetRandomFloat(StartPosition.Y, EndPosition.Y), Z:=Z}
            newTime:float = GetRandomFloat(MinTime, MaxTime)

            if (T:=Tornados[i]) 
            then {
                Print("Get i [{i}] Tornado from array")
                spawn {InitTornadoMoving(T, newVector, newTime)}
                }
            else Print("Can not get I device from array")

    
    InitTornadoMoving(InTornado:vfx_spawner_device, Destination:vector3, Time:float)<suspends>:void=
        Print("Init tornado moving")
        moveResult:=InTornado.MoveTo(Destination, IdentityRotation(), Time)
        set MoveResults += array{moveResult}


    InitializeZAxisVal():void=
        if(T:=Tornados[0], TR:=T.GetTransform().Translation) 
        then {
            set Z = TR.Z
            Print("Tornados Z is [{Z}]")
        } 
        else Print("Failed to find first Tornado and get transform")


    MoveResultChecker()<suspends>:void=
        loop:
            Print("Sleep before check move results status")
            Sleep(MinTime)

            for(Ind:int = 0..Tornados.Length):
                if(MR := MoveResults[Ind]):
                    # Print("MOVE RESULT [{MR}]")
                    if (MR = move_to_result.DestinationReached):
                        Print("Destination reached - spawn new Tornado Moving")
                        spawn { MoveTornadoInNewDestination(Ind) }

                        
        Print("Check Move Results finished")


    ToString(InMoveResult:move_to_result):string=
        return case(InMoveResult):
            move_to_result.DestinationReached => "DestinationReached"
            move_to_result.WillNotReachDestination => "WillNotReachDestination"


    InitializeZoneTransforms():void=
        if:
            set StartPosition = Points[0].GetTransform().Translation
            set EndPosition = Points[1].GetTransform().Translation
        then:
            Print("Tornados Zone Initilized")
            Print("Start Position [{StartPosition}]")
            Print("End Position [{EndPosition}]")
        else:
            Print("Tornados Zone is NOT Initilized")


    MoveTornadoInNewDestination(InTornadoNum:int)<suspends>:void=
        Print("Move i [{InTornadoNum}] tornado in new destination")
        if (set MoveResults[InTornadoNum] = move_to_result.WillNotReachDestination)
        then Print("Set Move Result in results array successfully")
        else Print("Set Move Result in results array Failed")

        newVector:vector3 = vector3{X:=GetRandomFloat(StartPosition.X, EndPosition.X), Y:=GetRandomFloat(StartPosition.Y, EndPosition.Y), Z:=Z}
        newTime:float = GetRandomFloat(MinTime, MaxTime)
        
        if(Tornado:=Tornados[InTornadoNum])
        then {
            moveResult:=Tornado.MoveTo(newVector, IdentityRotation(), newTime)
            if (set MoveResults[InTornadoNum] = moveResult) 
            then Print("Set Move Result in results array successfully")
            else Print("Set Move Result in results array Failed")
        }
        else Print("Fail to get Tornado by index")
        
        


