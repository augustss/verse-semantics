
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation}
using { /UnrealEngine.com/Temporary/Diagnostics }

log_sentry_device:=class(log_channel){}

# This is a new Verse based class that is used with the ScriptDevice in UEFN
# 
# Getting started: <link to UEFN docs>
device_station_sentry := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_sentry_device}

    @editable; SwitchSpawnDestroy:switch_device := switch_device{}
    @editable; Sentry:sentry_device := sentry_device{}

    var SpawnSwitchEventSubscription:?cancellable := false
    var DestroySwitchEventSubscription:?cancellable := false

    # Runs when this device_script is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Verse device started!")

        set SpawnSwitchEventSubscription = option{ SwitchSpawnDestroy.TurnedOffEvent.Subscribe(HandleSwitchSpawn) }
        set DestroySwitchEventSubscription = option{ SwitchSpawnDestroy.TurnedOnEvent.Subscribe(HandleSwitchDestroy) }

    HandleSwitchSpawn():void=
        Logger.Print("Verse: Sentry Should Spawn")
        Sentry.Spawn()
        
    HandleSwitchDestroy():void=
        Logger.Print("Verse: Sentry Should Be Destroyed")
        Sentry.DestroySentry()
  
    <#
    # Runs when this device_script is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Log("Verse device stopped!")
    #>
