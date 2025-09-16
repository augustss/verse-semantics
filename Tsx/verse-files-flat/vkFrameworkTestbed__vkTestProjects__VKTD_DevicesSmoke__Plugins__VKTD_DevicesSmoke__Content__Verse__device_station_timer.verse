
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation}
using { /UnrealEngine.com/Temporary/Diagnostics }

log_timer_device:=class(log_channel){}

# This is a new Verse based class that is used with the ScriptDevice in UEFN
# 
# Getting started: <link to UEFN docs>
device_station_timer := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_timer_device}

    @editable; PlayerSpawnerDevice:player_spawner_device := player_spawner_device{}
    @editable; ButtonStartDevice:button_device := button_device{}
    @editable; ButtonResetDevice:button_device := button_device{}
    @editable; SwitchPauseResume:switch_device := switch_device{} 
    @editable; TimerDevice:timer_device := timer_device{}

    var PlayerSpawnedSubscription:?cancellable := false
    var StartButtonEventSubscription:?cancellable := false
    var PauseSwitchEventSubscription:?cancellable := false
    var ResetButtonEventSubscription:?cancellable := false
    var ResumeSwitchEventSubscription:?cancellable := false
    var PlayerScriptReference:?player := false

    # Runs when this device_script is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Verse device started!")

        set PlayerSpawnedSubscription = option { PlayerSpawnerDevice.PlayerSpawnedEvent.Subscribe(HandlePlayerSpawned) }
        set StartButtonEventSubscription = option{ ButtonStartDevice.InteractedWithEvent.Subscribe(HandleStartButton) }
        set ResetButtonEventSubscription = option{ ButtonResetDevice.InteractedWithEvent.Subscribe(HandleResetButton) }

        set PauseSwitchEventSubscription = option{ SwitchPauseResume.TurnedOffEvent.Subscribe(HandleSwitchPause) }
        set ResumeSwitchEventSubscription = option{ SwitchPauseResume.TurnedOnEvent.Subscribe(HandleSwitchResume) }

    HandlePlayerSpawned(Player:player):void=
        Logger.Print("Verse: HandlePlayerSpawned ")
        set PlayerScriptReference = option { Player }
    
    HandleStartButton(Player:player):void=
        #ShowButtonEventSubscription?.Cancel() || true
        Logger.Print("Verse: Timer Should Start")
        TimerDevice.Start(Player)

    HandleResetButton(Player:player):void=
        #HideButtonEventSubscription?.Cancel() || true
        Logger.Print("Verse: Timer Should Reset")
        TimerDevice.Reset(Player)
    
    #need to pass a player ref instance around
    HandleSwitchPause():void=
        #HideButtonEventSubscription?.Cancel() || true
        Logger.Print("Verse: Timer Should Pause")
        if (Player := PlayerScriptReference?):
            TimerDevice.Pause(Player)
        #TimerDevice.PauseForAll()
    
    HandleSwitchResume():void=
        #HideButtonEventSubscription?.Cancel() || true
        Logger.Print("Verse: Timer Should Resume")
        if (Player := PlayerScriptReference?):
            TimerDevice.Resume(Player)
        #TimerDevice.ResumeForAll()

    <#
    # Runs when this device_script is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Log("Verse device stopped!")
    #>
