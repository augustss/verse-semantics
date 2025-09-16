
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation}
using { /UnrealEngine.com/Temporary/Diagnostics }

log_campfire_device:=class(log_channel){}

# This is a new Verse based class that is used with the ScriptDevice in UEFN
# 
# Getting started: <link to UEFN docs>
device_station_campfire := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_campfire_device}

    @editable; PlayerSpawnerDevice:player_spawner_device := player_spawner_device{}
    @editable; SwitchLightExtinguish:switch_device := switch_device{} 
    @editable; CampfireDevice:campfire_device := campfire_device{}

    var PlayerSpawnedSubscription:?cancellable := false
    var LightSwitchEventSubscription:?cancellable := false
    var ExtinguishSwitchEventSubscription:?cancellable := false

    var PlayerScriptReference:?player := false

    # Runs when this device_script is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Verse device started!")

        set LightSwitchEventSubscription = option{ SwitchLightExtinguish.TurnedOnEvent.Subscribe(HandleSwitchLight) }
        set ExtinguishSwitchEventSubscription = option{ SwitchLightExtinguish.TurnedOffEvent.Subscribe(HandleSwitchExtinguish) }
        set PlayerSpawnedSubscription = option { PlayerSpawnerDevice.PlayerSpawnedEvent.Subscribe(HandlePlayerSpawned) }

    HandlePlayerSpawned(Player:player):void=
        Logger.Print("Verse: HandlePlayerSpawned ")
        set PlayerScriptReference = option { Player }

    HandleSwitchLight():void=
        Logger.Print("Verse: Campfire Should Light")
        if (Player := PlayerScriptReference?):
            CampfireDevice.Light(Player)

    HandleSwitchExtinguish():void=
        Logger.Print("Verse: Campfire Should Be Extinguished")
        if (Player := PlayerScriptReference?):
            CampfireDevice.Extinguish(Player)
    <#
    # Runs when this device_script is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Log("Verse device stopped!")
    #>
