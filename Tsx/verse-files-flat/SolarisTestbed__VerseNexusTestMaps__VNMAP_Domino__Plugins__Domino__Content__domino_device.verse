using { /Fortnite.com/Characters }
using { /Fortnite.com/Devices }
using { /Fortnite.com/Game }
using { /Fortnite.com/Playspaces }
using { /Fortnite.com/Vehicles }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation }
using { /Fortnite.com/FortPlayerUtilities }

domino_device := class(creative_device):

    @editable
    Sequencer : cinematic_sequence_device = cinematic_sequence_device{}

    @editable    
    StartTrigger : trigger_device = trigger_device{}

    @editable
    PropToBuy: creative_prop = creative_prop {}

    @editable
    Barrier : barrier_device = barrier_device{}

    Vector : vector3 = vector3{X:=19.0, Y:=45.0, Z:=580.0}

    @editable
    Score : score_manager_device = score_manager_device{}

    @editable
    Sentinel : sentry_device = sentry_device{}

    @editable
    Spawn : player_spawner_device = player_spawner_device{}

    @editable
    FireTrapCollectible : collectible_object_device = collectible_object_device{}

    #@editable
        #FireTrap : fire_volume_device = fire_volume_device{}

    #@editable    
        #TrapDevice : creative_prop = creative_prop{}

    #@editable
        #HammerAnim : cinematic_sequence_device = cinematic_sequence_device{} 

    #@editable
        #Pinball : vehicle_spawner_baller_device = vehicle_spawner_baller_device{} 

    #@editable
        #Cage : barrier_device = barrier_device{}

    #@editable
        #Bomb : explosive_device = explosive_device {}

    var Bvar : int = 0
    var Plr : ?agent = false
    var Character : ?fort_character = false
    var Zparam : float = 1000.0
    var LoadingArray : []vector3 = array{}   
    var LoadBar : creative_prop = creative_prop{}
    var Speed : float = 0.0
    var Health : float = 0.0
    var MinSpeed : float = 5.0
    var LastPosition : vector3 = vector3{}
    var CurrentPosition : vector3 = vector3{}
    #var LoadingArr : []vector3 = array{vector3{X:=19.0, Y:=45.0, Z:=580.0}, vector3{X:=16.0, Y:=45.0, Z:=580.0}}

    OnBegin<override>()<suspends>:void=
        Print("Nexus OnBegin - domino_device")
        Sequencer.Play()
        Print("{PropToBuy.GetTransform().Translation}")

        Sentinel.Disable()

        Sleep(3.0)
        StartTrigger.TriggeredEvent.Subscribe(OnTrigger)

        # Try to freeze character from the start - FORT-695189 bug 
        #if (FortCharacter := Character?):
            #Print("Freeze char")
            #FortCharacter.PutInStasis(stasis_args{AllowFalling := true, AllowEmotes := false, AllowTurning := false})
        
    OnEnd<override>():void=
        Print("Nexus OnEnd - domino_device")

    GetPlayer(Agent : agent):void=
        set Plr = option{Agent}
        set Character = option{Agent.GetFortCharacter[]}
            
    OnTrigger(OptionalAgent : ?agent):void=
        if (Agent := OptionalAgent?):
            SleepTrigger(Agent)
            Score.Increment(Agent)
        
    SleepTrigger(Agent : agent):void=        
        if (Barrier.TeleportTo[Vector, rotation{}]):
            Print("Teleport works")
            TrapActivate(Agent)

    TrapActivate(Agent : agent):void=
        Spawn.Disable()

        if (CH := Agent.GetFortCharacter[]):
            if:
                CH.TeleportTo[FireTrapCollectible.GetTransform().Translation, FireTrapCollectible.GetTransform().Rotation]
            then:
                Print("Disabling fire trap out of pity for you")
            else: 
                Print("NexusError - Char found, teleport didn't work in TrapActivate() of domino_device")
        else:
            Print("NexusError - Char not found in TrapActivate() of domino_device") 

    # SleepFire(Player : ?agent)<suspends>:void=
    #     FireTrap.Disable()
    #     FireTrap.Ignite()
    #     Sleep(5.0)
    #     Print("I changed my mind")
    #     FireTrap.Enable()
    #     FireTrap.Ignite()
    #     #JusticeFromAbove()   

    # JusticeFromAbove()<suspends>:void=
    #     Sleep(10.0)

    #     loop:
    #         if (CH := Character?):
    #             if (TrapDevice.TeleportTo[CH.GetTransform().Translation + vector3{X:=0.0, Y:=-200.0, Z:=Zparam}, R]):
    #                 Print("It's coming")
    #                 set Zparam -= 2.0
    #                 Sleep(0.0)
    #             else: 
    #                 Print("Trap is broken") 

    #             if (Zparam <= 0.0):
    #                 break

    #     Sleep(2.0)          

    #     PinballDeath(Plr)

        #LoadingWithArrays(Plr)

    <#LoadingWithArrays(Player:?agent):void=
        
        VectorL:vector3=vector3{X:=-345.0, Y:=5360.0, Z:=1405.0}
        VectorO:vector3=vector3{X:=-345.0, Y:=5360.0, Z:=1405.0}
        VectorA:vector3=vector3{X:=-345.0, Y:=5360.0, Z:=1405.0}
        VectorD:vector3=vector3{X:=-345.0, Y:=5360.0, Z:=1405.0}
        VectorI:vector3=vector3{X:=-345.0, Y:=5360.0, Z:=1405.0}
        VectorN:vector3=vector3{X:=-345.0, Y:=5360.0, Z:=1405.0}
        VectorG:vector3=vector3{X:=-345.0, Y:=5360.0, Z:=1405.0}

        set LoadingArray = array{VectorL, VectorO, VectorA, VectorD, VectorI, VectorN, VectorG}
        #set LoadBar = Meshes.Box_F6F2A2D6
        Print("before load")
        for (Load := 0..6):

            Print("Loading")

            #Meshes.Box_F6F2A2D6
            if:
                LoadBar.TeleportTo[LoadingArray[Load], R]#>
    
    #PinballDeath(Player : ?agent)<suspends>:void=
        #Pinball:vehicle_spawner_device = vehicle_spawner_baller_device{}  

        #if (P := Player?):
            #Pinball.AssignDriver(P)
            #Print("EJECT!")
            #HammerAnim.Play()
            #Sleep(2.5) //Moved to capture_zones_device 
            #Cage.Enable()
            #ElimIfBelowSpeed(P)

    # GetSpeed(FortCharacter : fort_character)<suspends>:float=
    #     set LastPosition = CurrentPosition
    #     Sleep(0.2)
    #     set CurrentPosition = FortCharacter.GetTransform().Translation
    #     PlayerDistance := DistanceXY(CurrentPosition, LastPosition)
    #     # Speed is Distance / Time. This gives us centimeters/second. Fortnite tracks speed using Kilometers so we need to convert that product
    #     SpeedKMH := 3.6 * ((PlayerDistance / 0.2) / 100.0)
    #     return SpeedKMH

    # ElimIfBelowSpeed<public>(Agent : agent)<suspends>:void=
    #     Sleep(3.0)
        
    #     loop:                
    #         if (FortCharacter := Agent.GetFortCharacter[]):
    #             set Speed = GetSpeed(FortCharacter)
    #             BombFollowPlayer(FortCharacter)
    #             Print ("Your speed is {Speed} Km / Hour")

    #             if (Speed < MinSpeed):
    #                 Bomb.Explode(Agent)
    #                 Bomb.Reset()
    #                 set Health = FortCharacter.GetHealth()
    #                 Print("Healt: {Health}")

    #                 if (Health = 98.0):
    #                     Print("Healt: {Health}")
    #                     break

    # BombFollowPlayer(FortCharacter : fort_character):void=
    #     if (Bomb.TeleportTo[FortCharacter.GetTransform().Translation, rotation{}]) {}