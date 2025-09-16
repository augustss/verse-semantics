
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation}
using { /UnrealEngine.com/Temporary/Diagnostics }

log_device_station_health_powerup:=class(log_channel){}

# This is a Verse-authored creative device that can be placed in a level
# 
# Getting started:
#   https://www.epicgames.com/fortnite/en-US/creative/docs/uefn/programming-with-verse-in-unreal-editor-for-fortnite
#
device_station_health_powerup := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_device_station_health_powerup}

    @editable; ButtonSpawnDevice:button_device := button_device{}
    @editable; ButtonDespawnDevice:button_device := button_device{}
    @editable; ButtonPickupDevice:button_device := button_device{}
    @editable; HealthPowerupDevice:powerup_device := powerup_device{}

    var SpawnButtonEventSubscription:?cancellable := false
    var DespawnButtonEventSubscription:?cancellable := false
    var PickupButtonEventSubscription:?cancellable := false

    # Runs when this creative_device is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Verse Health Powerup Station device started!")

        set SpawnButtonEventSubscription = option{ ButtonSpawnDevice.InteractedWithEvent.Subscribe(HandleSpawnButton) }
        set DespawnButtonEventSubscription = option{ ButtonDespawnDevice.InteractedWithEvent.Subscribe(HandleDespawnButton) }
        set PickupButtonEventSubscription = option{ ButtonPickupDevice.InteractedWithEvent.Subscribe(HandlePickupButton) }

    HandleSpawnButton(Player:player):void=
        Logger.Print("Verse: Health Powerup Should Spawn")
        HealthPowerupDevice.Spawn()

    HandleDespawnButton(Player:player):void=
        Logger.Print("Verse: Health Powerup Should Despawn")
        HealthPowerupDevice.Despawn()

    HandlePickupButton(Player:player):void=
        Logger.Print("Verse: Health Powerup Should Get Picked Up")
        HealthPowerupDevice.Pickup(Player)
    <#
    # Runs when this creative_device is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Logger.Print("Verse device stopped!")
    #>
