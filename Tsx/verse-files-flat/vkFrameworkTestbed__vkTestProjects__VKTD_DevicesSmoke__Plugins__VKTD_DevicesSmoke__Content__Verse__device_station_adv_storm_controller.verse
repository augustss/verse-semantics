
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

log_advanced_storm_controller_device:=class(log_channel){}

# This is a new Verse based class that is used with the ScriptDevice in UEFN
# 
# Getting started: <link to UEFN docs>
device_station_adv_storm_controller := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_advanced_storm_controller_device}

    @editable; PlayerSpawnerDevice:player_spawner_device := player_spawner_device{}
    @editable; SwitchVerseControlStorm:switch_device = switch_device{}
    @editable; SwitchControlStormNormal:switch_device = switch_device{}
    @editable; AdvancedStormController:advanced_storm_controller_device = advanced_storm_controller_device{}

    var PlayerSpawnedSubscription:?cancellable := false
    var SwitchVerseTurnedOnEventSubscription:?cancellable := false
    var SwitchVerseTurnedOffEventSubscription:?cancellable := false
    var SwitchMainTurnedOnEventSubscription:?cancellable := false
    var SwitchMainTurnedOffEventSubscription:?cancellable := false

    var PlayerScriptReference:?player := false

    # Runs when this device_script is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Verse device started!")

        set PlayerSpawnedSubscription = option { PlayerSpawnerDevice.PlayerSpawnedEvent.Subscribe(HandlePlayerSpawned) }
        set SwitchVerseTurnedOffEventSubscription = option { SwitchVerseControlStorm.TurnedOffEvent.Subscribe(HandleVerseSwitchTurnedOn) }
        set SwitchVerseTurnedOnEventSubscription = option { SwitchVerseControlStorm.TurnedOnEvent.Subscribe(HandleVerseSwitchTurnedOff) }
        set SwitchMainTurnedOnEventSubscription = option { SwitchControlStormNormal.TurnedOffEvent.Subscribe(HandleMainSwitchTurnedOn) }
        set SwitchMainTurnedOffEventSubscription = option { SwitchControlStormNormal.TurnedOnEvent.Subscribe(HandleMainSwitchTurnedOff) }

    HandlePlayerSpawned(Player:player):void=
        Logger.Print("Verse: HandlePlayerSpawned ")
        set PlayerScriptReference = option { Player }
    
    HandleVerseSwitchTurnedOn():void=
        Logger.Print("Verse: Advanced Storm Should Be Gettting Turned on through Verse")
        AdvancedStormController.GenerateStrom()
        if (Player := PlayerScriptReference?):
            SwitchControlStormNormal.TurnOff(Player)
    
    HandleVerseSwitchTurnedOff():void=
        Logger.Print("Verse: Advanced Storm Should Be Gettting Destroyed through Verse")
        AdvancedStormController.DestoryStorm()
        if (Player := PlayerScriptReference?):
            SwitchControlStormNormal.TurnOn(Player)

    HandleMainSwitchTurnedOn():void=
        Logger.Print("Verse: Advanced Storm Has Been Turned on through a Switch with Event Bindings, so Verse Switch should be toggled")
        if (Player := PlayerScriptReference?):
            SwitchVerseControlStorm.TurnOff(Player)
    
    HandleMainSwitchTurnedOff():void=
        Logger.Print("Verse: Advanced Storm Has Been Turned off through a Switch with Event Bindings, so Verse Switch should be toggled")
        if (Player := PlayerScriptReference?):
            SwitchVerseControlStorm.TurnOn(Player)

    <#
    # Runs when this device_script is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Log("Verse device stopped!")
    #>
