
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Random }
using { /Verse.org/Colors }

tornado := class<concrete>:

    @editable
    VFXTornado:vfx_spawner_device=vfx_spawner_device{}

    @editable
    DamageVolume:damage_volume_device = damage_volume_device{}

    var NewTornadoLocation : vector3 = vector3{}

    var DirectionMovingIn : direction = direction.Up


tornado_trap_device := class(obstacle_device):

    TornadoBaseZ:float = 60.0 # Hard coded since this will never change

    @editable
    Tornados:[]tornado=array{}

    TornadoMinMaxPositions:min_max_vector3= min_max_vector3{Minimum:=vector3{X:=-3159.0,Y:=-3998.096428, Z:=3000.0}, Maximum:=vector3{X:=3000.0, Y:=4000.0, Z:=4000.0}}
    var HalfwayMarkXY:vector2 = vector2{}

    var ZeroRotation:rotation=rotation{}

    @editable
    var TornadoSpeed:float = 50.0

    # OnBegin<override>()<suspends>:void=

    #     set ZeroRotation = MakeRotation(vector3{X:=0.0,Y:=0.0,Z:=0.0}, 0.0)
    #     set HalfwayMarkXY = vector2{X:=TornadoMinMaxPositions.Maximum.X+TornadoMinMaxPositions.Minimum.X, Y:=TornadoMinMaxPositions.Maximum.Y+TornadoMinMaxPositions.Minimum.Y}

    #     RandomlyPlaceTornados()
    #     CheckForNewTornadoLocation()


    # only for the start of the game
    RandomlyPlaceTornados()<suspends>:void=

        for(Tornado:Tornados):
            Print("Tornado Initialized", ?Color:=NamedColors.Magenta)
            NewTornadoLocation(Tornado)
            MoveTask := spawn{MoveTornado(Tornado, Tornado.NewTornadoLocation)}

    CheckForNewTornadoLocation()<suspends>:void=

        loop:
            Sleep(0.0)
            for(Tornado:Tornados):


                var NeedsNewLocation:logic = false
                TornadoPosition:=Tornado.VFXTornado.GetTransform().Translation
                TornadoDestination:=Tornado.NewTornadoLocation
                case(Tornado.DirectionMovingIn):
                    direction.Up =>
                        if(TornadoPosition.Y >= TornadoDestination.Y-100.0){set NeedsNewLocation = true}
                    direction.Right =>
                        if(TornadoPosition.X >= TornadoDestination.X-100.0){set NeedsNewLocation = true}
                    direction.Left =>
                        if(TornadoPosition.X <= TornadoDestination.X+100.0){set NeedsNewLocation = true}
                    direction.Down =>
                        if(TornadoPosition.Y <= TornadoDestination.Y+100.0){set NeedsNewLocation = true}
        
                if(NeedsNewLocation?):
                    NewTornadoLocation(Tornado)
                    MoveTornado(Tornado, Tornado.NewTornadoLocation, Tornado.DirectionMovingIn)



# this does the actual moving
    MoveTornado(TornadoToMove:tornado,NewPosition:vector3)<suspends>:void=

        TornadoToMove.VFXTornado.MoveTo(NewPosition, ZeroRotation, 1.0)
        TornadoToMove.DamageVolume.MoveTo(NewPosition, ZeroRotation, 1.0)

    MoveTornado(TornadoToMove:tornado,NewPosition:vector3, Direction:direction):void=
        Print("Tornado Moving!", ?Color:=NamedColors.Green)

        var NextPosition:vector3=NewPosition

        case(Direction):
            direction.Up =>
                set NextPosition=AddFloatToVector(vector.Y, NextPosition, TornadoSpeed)
            direction.Right =>
                set NextPosition=AddFloatToVector(vector.X, NextPosition, TornadoSpeed)
            direction.Left =>
                set NextPosition=AddFloatToVector(vector.X, NextPosition, -TornadoSpeed)
            direction.Down =>
                set NextPosition=AddFloatToVector(vector.Y, NextPosition, -TornadoSpeed)

        MoveTornadoTask:=spawn{MoveTornado(TornadoToMove, NewPosition)}

    @editable
    AboveHalfwayThresholdFloat:float = 500.0
    NewTornadoLocation(TornadoToSet:tornado):void=

        var DirectionsThatCanMoveIn:[]direction = array{}

        TornadoXPosition:=TornadoToSet.VFXTornado.GetTransform().Translation.X
        TornadoYPosition:=TornadoToSet.VFXTornado.GetTransform().Translation.Y

        if(TornadoXPosition>HalfwayMarkXY.X){ set DirectionsThatCanMoveIn += array{direction.Left} }else{ set DirectionsThatCanMoveIn += array{direction.Right }}
        if(TornadoYPosition>HalfwayMarkXY.Y){ set DirectionsThatCanMoveIn += array{direction.Down} }else{ set DirectionsThatCanMoveIn += array{direction.Up }}

        RandomDirectionInt := GetRandomInt(0, DirectionsThatCanMoveIn.Length)
        
        var NewYLocation:float = 0.0
        var NewXLocation:float = 0.0


        if(ChosenDirection:direction = DirectionsThatCanMoveIn[RandomDirectionInt]):

            case(ChosenDirection):
                direction.Up =>
                    set NewYLocation= GetRandomFloat(HalfwayMarkXY.Y+AboveHalfwayThresholdFloat, TornadoMinMaxPositions.Maximum.Y)
                    set NewXLocation=TornadoXPosition
                direction.Right =>
                    set NewXLocation= GetRandomFloat(HalfwayMarkXY.X+AboveHalfwayThresholdFloat, TornadoMinMaxPositions.Maximum.X)
                    set NewYLocation= TornadoYPosition
                direction.Left =>
                    set NewXLocation= GetRandomFloat(HalfwayMarkXY.X-AboveHalfwayThresholdFloat, TornadoMinMaxPositions.Minimum.X)
                    set NewYLocation= TornadoYPosition
                direction.Down =>
                    set NewYLocation= GetRandomFloat(HalfwayMarkXY.Y-AboveHalfwayThresholdFloat,TornadoMinMaxPositions.Minimum.Y)
                    set NewXLocation= TornadoXPosition

                

            RandomLocation:vector3=vector3 {X:=NewXLocation, Y:=NewYLocation, Z:= TornadoBaseZ} 
            set TornadoToSet.NewTornadoLocation = RandomLocation
            set TornadoToSet.DirectionMovingIn = ChosenDirection



    RandomDirectionEnum():direction=

        DirectionAsInt:int= GetRandomInt(0,4)

        case(DirectionAsInt):
            0 => 
                direction.Up
            1 => 
                direction.Left
            2 =>
                direction.Right
            3 =>
                direction.Down
            _ =>
                direction.Up


    BeginRound<override>()<suspends>:void=

        set ZeroRotation = MakeRotation(vector3{X:=0.0,Y:=0.0,Z:=0.0}, 0.0)
        set HalfwayMarkXY = vector2{X:=TornadoMinMaxPositions.Maximum.X+TornadoMinMaxPositions.Minimum.X, Y:=TornadoMinMaxPositions.Maximum.Y+TornadoMinMaxPositions.Minimum.Y}

        RandomlyPlaceTornados()
        CheckForNewTornadoLocation()