using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Fortnite.com/Characters }
using { /Verse.org/Simulation }
using { /Fortnite.com/Devices }
using { /Verse.org/Random }
using { /Fortnite.com/Vehicles }

using { Interfaces }
using { Controllers }
using { Systems }


PlayerWrapper<public> := class<unique>(ITickable):

    FortCharacter<public>:fort_character
    Agent<public>:agent
    FortPlayer<public>:player

    var CharacterTransform<public>:transform = transform{}

    LocalRain<public>:vfx_spawner_device
    Lighting<public>:LightingConfig
    Leaves<public>:LeavesConfig

    var leavesIsEnabled<private>:logic = false

    var Vehicle<public>:?fort_vehicle = false


    var IsVehicleOnPitStop<public>:logic = false

    vehicleHealRate<private>:float = 10.0
    vehicleHealTimeOut<private>:float = 1.0

    var IsVehicleHealingWatcherStarted<private>:logic = false

    var CheckPoints<public>:?CheckPointsSystem = false


    Tick<override>()<suspends>:void=
        UpdatePlayerTransform()
        LocalRainUpdate()
        LightingPositionUpdate()
        LeavesPositionUpdate()

        if(IsVehicleHealingWatcherStarted = false):
            spawn{VehicleHealingServiceWatcher()}
        

    UpdatePlayerTransform<private>():void=
        set CharacterTransform = FortCharacter.GetTransform()
        # Print("CHARACTER TRANSLATION: [{CharacterTransform.Translation}]")

    
    LocalRainUpdate():void=
        if(LocalRain.TeleportTo[CharacterTransform]){}
        # then Print("LOCAL RAIN IS TELEPORTED - TRANSLATION: [{LocalRain.GetTransform().Translation}]")
        # else Print("LOCAL RAIN IS NOT TELEPORTED - TRANSLATION: [{LocalRain.GetTransform().Translation}]")

       
    LightingSwitchUpdate<public>()<suspends>:void=
        Print("Lighting Switching Starts")
        loop:
            Lighting.LightingDevice.Enable()
            Sleep(Lighting.TimeOut)
            Lighting.LightingDevice.Disable()
            Sleep(Lighting.TimeOut)


    LightingPositionUpdate():void=
        DeltaX:float = GetRandomFloat(-Lighting.DeltaShift, Lighting.DeltaShift)
        DeltaY:float = GetRandomFloat(-Lighting.DeltaShift, Lighting.DeltaShift)
        # DeltaX := 0.0
        # DeltaY := 0.0
        SpawnPosition:vector3 = vector3{X := (CharacterTransform.Translation.X + DeltaX), Y := (CharacterTransform.Translation.Y + DeltaY), Z := CharacterTransform.Translation.Z}
        if (Lighting.LightingDevice.TeleportTo[SpawnPosition, IdentityRotation()]){}


    LeavesPositionUpdate():void=
        if (leavesIsEnabled = false):
            Leaves.LeavesDevice.Enable()
            set leavesIsEnabled = true
        Position:vector3 = vector3{X := (CharacterTransform.Translation.X - Leaves.DeltaShift), Y := CharacterTransform.Translation.Y, Z := CharacterTransform.Translation.Z}
        # Print("PLAYER ROTATION [{CharacterTransform.Rotation}]")
        if (Leaves.LeavesDevice.TeleportTo[Position, CharacterTransform.Rotation]){}
        # Print("LEAVES ROTATION [{Leaves.LeavesDevice.GetTransform().Rotation}]")


    VehicleHealingServiceWatcher()<suspends>:void=
        Print("Starts Vehicle Healing Watcher")
        set IsVehicleHealingWatcherStarted = true
        loop:
            # Print("Check Is Vehicle On Pit Stop")
            if (IsVehicleOnPitStop = true):
                Print("Vehicle Is On Pit Stop")
                if(var vhp:float = Vehicle?.GetHealth(), 
                    set vhp += vehicleHealRate, 
                    Vehicle?.SetHealth(vhp))
                then {
                    Print("Vehicle Exists")
                    Print("Vehicle Get Health Success")
                    Print("Vehicle New Health [{vhp}]")
                    Print("Vehicle Set Health Success")
                    }
                else {
                    Print("Vehicle Is Not Exists in Optional")
                }
            Sleep(vehicleHealTimeOut)



ConstructPlayer<constructor><public>(InFortCharacter:fort_character, InAgent:agent, InPlayer:player, InRain:vfx_spawner_device, InLighting:LightingConfig, InLeaves:LeavesConfig) := PlayerWrapper:
    FortCharacter := InFortCharacter
    Agent := InAgent
    FortPlayer := InPlayer
    LocalRain := InRain
    Lighting := InLighting
    Leaves := InLeaves
