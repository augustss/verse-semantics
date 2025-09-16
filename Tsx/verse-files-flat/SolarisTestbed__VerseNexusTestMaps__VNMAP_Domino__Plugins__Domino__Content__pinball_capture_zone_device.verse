using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Fortnite.com/Game }

pinball_capture_zone_device := class(creative_device):

    @editable
    Cage : barrier_device = barrier_device{}

    @editable
    CageCaptureZone : capture_area_device = capture_area_device{}
   
    @editable
    Bomb : explosive_device = explosive_device {}

    @editable
    Speedometer : speedometer_device = speedometer_device{}

    @editable
    HammerAnim : cinematic_sequence_device = cinematic_sequence_device{} 

    @editable
    Pinball : vehicle_spawner_baller_device = vehicle_spawner_baller_device{} 

    @editable
    HouseTeleporter : teleporter_device = teleporter_device{}

    var CurrentPosition<private> : vector3 = vector3{}
    var MinSpeed<private>  : float = 20.0
    var IsBombShouldExplode<private> : logic = true
    var IsLoopShouldEnd<private> : logic = false
    var IsCharacterInBaller<protected        > : logic = false
    var Health<private> : float = 0.0

    OnBegin<override>()<suspends>:void=
        Sleep(1.0)
        Print("Nexus OnBegin - pinball_capture_zone_device")
        CageCaptureZone.Enable()
        Cage.Disable()
        Pinball.OutOfEnergyEvent.Subscribe(RefillEnergy)
        CageCaptureZone.AgentEntersEvent.Subscribe(EliminatePlayer)
        Bomb.ExplodedEvent.Subscribe(OnBombExploded)
        Pinball.AgentEntersVehicleEvent.Subscribe(CharacterInBaller)
       
    OnEnd<override>():void=
        Print("Nexus OnEnd - pinball_capture_zone_device")

    CharacterInBaller(Agent : agent):void=
        set IsCharacterInBaller = true

    RefillEnergy(Tuple : tuple()):void=
        Print("Energy refilled")
        Pinball.RefillEnergy()
       
    EliminatePlayer(Agent : agent):void=
        Cage.Enable()
        spawn{ElimIfBelowSpeed(Agent)}
    
    GetSpeed(FortCharacter : fort_character)<suspends>:float=
        LastPosition := CurrentPosition
        Sleep(0.2)
        set CurrentPosition = FortCharacter.GetTransform().Translation
        PlayerDistance := DistanceXY(CurrentPosition, LastPosition)
        # Speed is Distance / Time. This gives us centimeters/second. Fortnite tracks speed using Kilometers so we need to convert that product
        SpeedKMH := 3.6 * ((PlayerDistance / 0.2) / 100.0)
        return SpeedKMH

    OnBombExploded(Agent : agent):void=
        Print("Bomb exploded")
        spawn{OnBombExplodedSuspends(Agent)}

    OnBombExplodedSuspends(Agent : agent)<suspends>:void=
        set IsBombShouldExplode = true

    OnEliminatedInPinball(Agent : agent)<suspends>:void=
            Speedometer.RemoveWidget(Agent) 
            if (FortCharacter := Agent.GetFortCharacter[], set Health = FortCharacter.GetHealth(), Health <> 0.0):
                Print("NexusError - Current health: {Health} when character eliminated in OnEliminatedInPinballSuspends() of pinball_capture_zone_device")

            TeleportToHouse(Agent)

    TeleportToHouse(Agent : agent)<suspends>:void=
            loop:
                if (FortCharacter := Agent.GetFortCharacter[]):
                    if (FortCharacter.IsActive[]):
                        break
                Sleep(0.5)  
            Print("Character will teleport")
            HouseTeleporter.Teleport(Agent)      
                
    ElimIfBelowSpeed<public>(Agent : agent)<suspends>:void=  
        loop:    
            if (FortCharacter := Agent.GetFortCharacter[]): 
                if (FortCharacter.IsActive[]):
                    Speed := GetSpeed(FortCharacter)
                    Speedometer.AddWidget(Agent, "Your speed is {Speed} Km/H")
                    if (Speed < MinSpeed):

                        BombFollowPlayer(FortCharacter)
                        Bomb.Explode(Agent)
                        Bomb.Reset()

                        set Health = FortCharacter.GetHealth()
                        Print("Health: {Health}")
    
                    if (FortCharacter.IsActive[]):
                        if:
                            Cage.GetTransform().Translation.Z > FortCharacter.GetTransform().Translation.Z
                        else:        
                            Print("NexusError - FortCharacter should be in cage in ElimIfBelowSpeed() of pinball_capture_zone_device")
                else:
                    spawn{OnEliminatedInPinball(Agent)}
                    break
          
            else:
                Print("NexusError - FortCharacter should be valid in ElimIfBelowSpeed() of pinball_capture_zone_device")     
                break           
                  

    BombFollowPlayer(FortCharacter : fort_character):void=
        if (Bomb.TeleportTo[FortCharacter.GetTransform().Translation, rotation{}]) {}
 
    StartPinballDeath<public>(Agent : agent)<suspends>:void=
        Print("Statring pinball zone")

        loop:
            if (IsCharacterInBaller = true):
                break;
            else:
                Print("Try to assign driver")
                Pinball.AssignDriver(Agent)
            Sleep(0.0)
        Print("EJECT!")
        HammerAnim.Play()