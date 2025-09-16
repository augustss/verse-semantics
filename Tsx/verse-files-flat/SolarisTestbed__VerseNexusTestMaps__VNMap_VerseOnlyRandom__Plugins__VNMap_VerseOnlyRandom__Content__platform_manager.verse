
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }

platform_manager := class(creative_device):
    # Platform variables
    @editable
    Spawner1: player_spawner_device = player_spawner_device {}
    @editable
    Platform1: creative_prop = creative_prop {}

    @editable
    Spawner2: player_spawner_device = player_spawner_device {}
    @editable
    Platform2: creative_prop = creative_prop {}

    @editable
    Spawner3: player_spawner_device = player_spawner_device {}
    @editable
    Platform3: creative_prop = creative_prop {}

    @editable
    Spawner4: player_spawner_device = player_spawner_device {}
    @editable
    Platform4: creative_prop = creative_prop {}
    
    @editable
    PlatformSpeed:float = 50.0
    @editable
    PlatformFinalZPos: float = 100000.0
    @editable
    RateOfChange: float = 0.3

    OnEnd<override>():void=
        Print ("Nexus OnEnd - platform_manager")

    OnBegin<override>()<suspends>:void=
        Print ("Nexus OnBegin - platform_manager")
        Print ("Platforms will move in 5 seconds")
        Spawner1.SpawnedEvent.Subscribe(Platform1StartEvent)
        Spawner2.SpawnedEvent.Subscribe(Platform2StartEvent)
        Spawner3.SpawnedEvent.Subscribe(Platform3StartEvent)
        Spawner4.SpawnedEvent.Subscribe(Platform4StartEvent)
        
    # Logic for moving the platform
    Platform1StartEvent (Agent: agent):void=
        spawn:
            MovePlatform1()
    Platform2StartEvent (Agent: agent):void=
        spawn:
            MovePlatform2()
    Platform3StartEvent (Agent: agent):void=
        spawn:
            MovePlatform3()
    Platform4StartEvent (Agent: agent):void=
        spawn:
            MovePlatform4()
    MovePlatform1()<suspends>:void=
        Print ("Platform 1 is moving up")
        StartPos: vector3 = Platform1.GetTransform().Translation
        FinalPos: vector3 = vector3{X:= StartPos.X, Y:= StartPos.Y, Z:= PlatformFinalZPos}
        # Delay or else the platform doesn't catch the player and they fall through
        Sleep(5.0)
        loop:
            var CurrentPos: vector3 = Platform1.GetTransform().Translation
            if(Distance(CurrentPos, FinalPos) > PlatformSpeed):
                NewZ: float = CurrentPos.Z + PlatformSpeed
                Platform1.MoveTo(vector3{X:= CurrentPos.X, Y:= CurrentPos.Y, Z:= NewZ}, Platform1.GetTransform().Rotation, RateOfChange) # Every % of a tick we move
            else:
                Print ("Platform 1 has reached it's destination")
                break:
    MovePlatform2()<suspends>:void=
        Print ("Platform 2 is moving up")
        StartPos: vector3 = Platform2.GetTransform().Translation
        FinalPos: vector3 = vector3{X:= StartPos.X, Y:= StartPos.Y, Z:= PlatformFinalZPos}
        # Delay or else the platform doesn't catch the player and they fall through
        Sleep(5.0)
        loop:
            var CurrentPos: vector3 = Platform2.GetTransform().Translation
            if(Distance(CurrentPos, FinalPos) > PlatformSpeed):
                NewZ: float = CurrentPos.Z + PlatformSpeed
                Platform2.MoveTo(vector3{X:= CurrentPos.X, Y:= CurrentPos.Y, Z:= NewZ}, Platform2.GetTransform().Rotation, RateOfChange) # Every % of a tick we move
            else:
                Print ("Platform 2 has reached it's destination")
                break:
    MovePlatform3()<suspends>:void=
        Print ("Platform 3 is moving up")
        StartPos: vector3 = Platform3.GetTransform().Translation
        FinalPos: vector3 = vector3{X:= StartPos.X, Y:= StartPos.Y, Z:= PlatformFinalZPos}
        # Delay or else the platform doesn't catch the player and they fall through
        Sleep(5.0)
        loop:
            var CurrentPos: vector3 = Platform3.GetTransform().Translation
            if(Distance(CurrentPos, FinalPos) > PlatformSpeed):
                NewZ: float = CurrentPos.Z + PlatformSpeed
                Platform3.MoveTo(vector3{X:= CurrentPos.X, Y:= CurrentPos.Y, Z:= NewZ}, Platform3.GetTransform().Rotation, RateOfChange) # Every % of a tick we move
            else:
                Print ("Platform 3 has reached it's destination")
                break:
    MovePlatform4()<suspends>:void=
        Print ("Platform 4 is moving up")
        StartPos: vector3 = Platform4.GetTransform().Translation
        FinalPos: vector3 = vector3{X:= StartPos.X, Y:= StartPos.Y, Z:= PlatformFinalZPos}
        # Delay or else the platform doesn't catch the player and they fall through
        Sleep(5.0)
        loop:
            var CurrentPos: vector3 = Platform4.GetTransform().Translation
            if(Distance(CurrentPos, FinalPos) > PlatformSpeed):
                NewZ: float = CurrentPos.Z + PlatformSpeed
                Platform4.MoveTo(vector3{X:= CurrentPos.X, Y:= CurrentPos.Y, Z:= NewZ}, Platform4.GetTransform().Rotation, RateOfChange) # Every % of a tick we move
            else:
                Print ("Platform 4 has reached it's destination")
                break: