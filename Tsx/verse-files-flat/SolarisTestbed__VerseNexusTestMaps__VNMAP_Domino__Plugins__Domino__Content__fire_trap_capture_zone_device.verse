using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Fortnite.com/Characters }
using { /Fortnite.com/Game }
using { /Verse.org/Concurrency }

fire_trap_capture_zone_device := class(creative_device):

    @editable
    NpcSpawners : []npc_spawner_device = array{}

    @editable    
    TrapDevice : creative_prop = creative_prop{}

    @editable
    FireTrap : fire_volume_device = fire_volume_device{}

    @editable
    FireTrapCaptureZone : capture_area_device = capture_area_device{}

    @editable
    PinballCaptureZoneDevice : pinball_capture_zone_device = pinball_capture_zone_device{}

    @editable
    Camera : gameplay_camera_fixed_point_device = gameplay_camera_fixed_point_device{}

    @editable
    HouseZone : house_zone_device = house_zone_device{}

    @editable
    Bomb : explosive_device = explosive_device {}

    @editable
    PhysicsBoulderDevice:physics_boulder_device = physics_boulder_device{}

    var Zparam<private> : float = 1000.0
    var IsPlayerEliminated<private> : logic = false
    var BoulderEventCounter : int = 0

    OnBegin<override>()<suspends>:void=
        Sleep(2.0)
        Print("Nexus OnBegin - fire_trap_capture_zone")
        FireTrapCaptureZone.Enable()
        FireTrapCaptureZone.AgentEntersEvent.Subscribe(FryPlayer)
        FireTrapCaptureZone.AgentExitsEvent.Subscribe(JusticeFromAbove)
        PhysicsBoulderDevice.RollingBoulderDestroyedEvent.Subscribe(OnRollingBoulderDestroyedEvent)
        PhysicsBoulderDevice.BaseDestroyedEvent.Subscribe(OnBaseDestroyedEvent)
        for(Spawner : NpcSpawners):
            Spawner.Enable()
            Spawner.SpawnedEvent.Subscribe(OnSpawned)
            
    OnEnd<override>():void=
        Print("Nexus OnEnd - fire_trap_capture_zone")

    OnSpawned(Agent : agent):void=
        for(Spawner : NpcSpawners):
            Spawner.Spawn()

    OnBaseDestroyedEvent():void=
        set BoulderEventCounter += 1
        Print("OnBaseDestroyedEvent")

    OnRollingBoulderDestroyedEvent():void=
        Print("OnRollingBoulderDestroyedEvent")
        set BoulderEventCounter += 1

    FryPlayer(Agent : agent):void=
        for(Spawner : NpcSpawners):
            Spawner.Spawn()
        Camera.AddTo(Agent)
        spawn{FryPlayerSuspends(Agent)}

    FryPlayerSuspends(Agent : agent)<suspends>:void=
        FireTrap.Disable()
        if (CH := Agent.GetFortCharacter[]):
            CurrentHealth := CH.GetHealth()
            FireTrap.Ignite()
            Sleep(3.0)
            if (CH.GetHealth() < CurrentHealth):
                Print("NexusError - Fire ignites after fire trap disabling in FryPlayerSuspends() of fire_trap_capture_zone_device")

        Print("I changed my mind")
        FireTrap.Enable()
        FireTrap.Ignite()
        spawn{ExtinguishFire(Agent)}
        PhysicsBoulderDevice.DestroyBase()
        Sleep(0.2)
        Bomb.Explode(Agent)
        if (not (BoulderEventCounter = 1)):
            Print("NexusError - some boulder event fired in fire_trap_capture_zone_device")

    ExtinguishFire<private>(Agent : agent)<suspends>:void=
        loop:    
            if (CH := Agent.GetFortCharacter[], CH.GetHealth() < 40.0):
                FireTrap.Extinguish()
                return
            Sleep(0.0)    

    JusticeFromAbove(Agent : agent):void=
        spawn{JusticeFromAboveSuspends(Agent)}

    SetPlayerEliminated(EliminationResult : elimination_result):void=
        set IsPlayerEliminated = true  

    JusticeFromAboveSuspends(Agent : agent)<suspends>:void= 
        defer:
            Camera.RemoveFrom(Agent)
            spawn{PinballCaptureZoneDevice.StartPinballDeath(Agent)}
            for(Spawner : NpcSpawners):
                Spawner.DespawnAll(option{Agent})

        if (CH := Agent.GetFortCharacter[]):
            CH.EliminatedEvent().Subscribe(SetPlayerEliminated)

            Print("It's coming")
            loop:
                if (IsPlayerEliminated = true):
                    if (CH.IsActive[]):
                        return 
                Sleep(0.0)

                if (TrapDevice.TeleportTo[CH.GetTransform().Translation + vector3{X:=0.0, Y:=0.0, Z:=Zparam}, CH.GetTransform().Rotation]):
                    set Zparam -= 10.0
                    Sleep(0.0) #This sleep is need for proper work of the justice functionality
                else: 
                    Print("NexusError - Trap is broken in JusticeFromAboveSuspends() of fire_trap_capture_zone_device") 
                    return

                if (CH.GetTransform().Translation.Z > Zparam):   
                    set Zparam = 1000.0 # If trap avoided and Z under character's Z then we need to "respawn" justice
                
                Sleep(0.0)