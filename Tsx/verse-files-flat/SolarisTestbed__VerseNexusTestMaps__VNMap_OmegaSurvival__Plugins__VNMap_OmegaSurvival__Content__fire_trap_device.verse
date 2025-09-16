
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Random }

# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

fire_trap_unit :=class<concrete>():
    @editable
    HighlightAsset:creative_prop=creative_prop{}

    @editable 
    FireTimer:timer_device=timer_device{}

    @editable
    FireDevice:fire_volume_device= fire_volume_device{}

    @editable
    var FireTimeFloat:float = 10.0

    var TimerPosition:vector3 = vector3{}

    var HasBeenInitialized:logic = false

    Initialize():void=
        # Print("Initialize being called")

        if(HasBeenInitialized = true){ return }
        set HasBeenInitialized = true

        HighlightAsset.Hide()

        set TimerPosition = FireTimer.GetTransform().Translation
        ShowTimer(false)

        # FireTimer.SetActiveDuration(WarningTime)
        FireTimer.SuccessEvent.Subscribe(ActivateFireTrap)
        FireTimer.Pause()


    # Start the fire trap timer
    StartFireTrapWarningTimer():void=
        HighlightAsset.Show()
        ShowTimer(true)

        # Print("Timer successfully teleported")
        FireTimer.Start()

    # Timer success event
    ActivateFireTrap(Agent:?agent):void=
        # Print("ActivateFireTrap being called")
        ActivateTrapTask:=spawn{StartFireTrap()}

    StartFireTrap()<suspends>:void=
        # Print("StartFireTrap being called")
        ShowTimer(false)
        HighlightAsset.Hide()

        FireDevice.Ignite()
        # FireDevice.Extinguish()

    ShowTimer(IsShow:logic):void=

        var ShowTimerFloat:float = 500.0
        Print("ShowTimer({LogicToString(IsShow)}) called")
        # Print("Location before moving = " + VectorToString(TimerPosition)) # Custom usability function

        if(IsShow = false) {set ShowTimerFloat*=-1.0}
        
        set TimerPosition = AddFloatToVector(vector.Z, TimerPosition, ShowTimerFloat) # Custom usability function # TimerPosition + vector3{X:=0.0,Y:=0.0, Z:=ShowTimerFloat} # Why can't this be set TimerPosition += vector3{X:=0.0,Y:=0.0, Z:=ShowTimerFloat}
        
        # Print("Location after moving = " + VectorToString(TimerPosition)) # Custom usability function

        Rot : rotation =  MakeRotation(vector3{X:=0.0,Y:=0.0,Z:=0.0}, 0.0)
        if(FireTimer.TeleportTo[TimerPosition, Rot]):
        else:
            Err("Error in  ShowTimer(IsShow:logic)::FireTimer.TeleportTo[TimerPosition, Rot]")


        

# A Verse-authored creative device that can be placed in a level
fire_trap_device := class(obstacle_device):

    @editable
    FireTrapUnits : []fire_trap_unit = array{}
    @editable
    DebugButton : button_device = button_device{}

    @editable
    FloorProp: creative_prop_asset = DefaultCreativePropAsset
    @editable
    FloorDespawnTime : float = 10.0

    RoundTime<override> : float = 10.0

    var BigVectorArray:[]vector3 = array{}

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        for(FireTrapUnit:FireTrapUnits):
            FireTrapUnit.Initialize()

        # DebugButton.InteractedWithEvent.Subscribe(ActivateFireTraps) # Can't currently subscribe to functions from other classes
        
    ActivateFireTraps(Agent:agent):void=
        for(FireTrapUnit:FireTrapUnits):
            FireTrapUnit.StartFireTrapWarningTimer()

        SpawnFloors:=spawn{ProceduralFloorSpawn()}


    ActivateFireTrapsNoParams()<suspends>:void=
        DespawnFloorTraps()
        for(FireTrapUnit:FireTrapUnits):
            FireTrapUnit.StartFireTrapWarningTimer()

        SpawnFloors:=spawn{ProceduralFloorSpawn()}


    Initialize():void=
        Print("")
        # subscribe timer to ActivateFireTrap
        # subscribe 

    
    var FloorProps : []?creative_prop = array{} 

    ProceduralFloorSpawn()<suspends>:void=
        Print("Floor Spawn Started")
        defer:
            Print("Floor Spawn Ended")
        Rot : rotation =  MakeRotation(vector3{X:=0.0,Y:=0.0,Z:=0.0}, 0.0)

        StartingPosition : vector3 = vector3{X:=-4159.0,Y:=3802.4}

        FloorHorizontalCount : int = 15
        FloorVerticalCount : int = 16
        FloorDepthCount : int = 2
        FloorLength : float = 512.0
        FloorDepth : float = 10.0

        var CurrentPosition : vector3 = StartingPosition

       
        for(DepthIndex:=0..FloorDepthCount):
            for(VerticalIndex:=0..FloorVerticalCount):
                for(HorizontalIndex:=0..FloorHorizontalCount):
                    if(DepthIndex <> 0 and GetRandomInt(0,2)=1):

                    else:
                        NewlySpawnedProp := SpawnProp(FloorProp, CurrentPosition, Rot)(0)
                        set FloorProps +=  array{NewlySpawnedProp,NewlySpawnedProp,NewlySpawnedProp,NewlySpawnedProp,NewlySpawnedProp,NewlySpawnedProp}
                        set CurrentPosition.X += FloorLength
                        set BigVectorArray += array{vector3{ X:= CurrentPosition.X, Y:= CurrentPosition.Y, Z:= CurrentPosition.Z}}

                set CurrentPosition.Y -= FloorLength
                set CurrentPosition.X = StartingPosition.X

            set CurrentPosition.Y = StartingPosition.Y
            set CurrentPosition.Z += FloorDepth

        # Sleep(FloorDespawnTime)

        # for(TheFloorProp : FloorProps, PropToDestroy:= TheFloorProp?):
        #     PropToDestroy.Dispose()
#    BeginRound<override>()<suspends>:void=
#     EndRound<override>()<suspends>:void=

    BeginRound<override>()<suspends>:void=
        loop:
            ActivateFireTrapsTask:=spawn{ActivateFireTrapsNoParams()}
            Sleep(GetRoundTime())
        # ActivateFireTraps()

    DespawnFloorTraps():void=
        Print("Floor Despawn Started")
        defer:
            Print("Floor Despawn Ended")
        for(TheFloorProp : FloorProps, PropToDestroy:= TheFloorProp?):
            PropToDestroy.Dispose()

    EndRound<override>()<suspends>:void=
        for(TheFloorProp : FloorProps, PropToDestroy:= TheFloorProp?):
            PropToDestroy.Dispose()