
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation } 
using { /UnrealEngine.com/Temporary/Diagnostics }

log_teleport_device:=class(log_channel){}

# This is a new Verse based class that is used with the ScriptDevice in UEFN
# 
# Getting started: <link to UEFN docs>
device_station_teleport := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_new_device}
    
    @editable; TeleportSkydive:teleporter_device := teleporter_device{}
    @editable; TeleportStart:teleporter_device := teleporter_device{}
    @editable; TeleportMid:teleporter_device := teleporter_device{}
    @editable; ButtonTeleportStart:button_device := button_device{}
    @editable; ButtonTeleportMid:button_device := button_device{}
    @editable; SwitchTeleporterDestination:switch_device := switch_device{}
    @editable; SkydiveTrigger:trigger_device := trigger_device{} 

    var TeleportStartEventSubscription:?cancellable := false
    var TeleportMidEventSubscription:?cancellable := false
    var SteppedonSkydiveTriggerEventSubscription:?cancellable := false

    # Runs when this device_script is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Verse device started!")

        set TeleportStartEventSubscription = option { TeleportStart.TeleportedEvent.Subscribe(HandlePlayerTeleportFromStart) }
        set TeleportMidEventSubscription = option { TeleportMid.TeleportedEvent.Subscribe(HandlePlayerTeleportFromMid) }
        set SteppedonSkydiveTriggerEventSubscription = option {SkydiveTrigger.TriggeredEvent.Subscribe(HandlePlayerStepOnSkydiveTrigger) }

    HandlePlayerTeleportFromStart(Player:player):void=
        Logger.Print("Verse: Player Teleported from Start")
        SwitchTeleporterDestination.TurnOff(Player)

    HandlePlayerTeleportFromMid(Player:player):void=
        Logger.Print("Verse: Player Teleported from Mid")
        SwitchTeleporterDestination.TurnOn(Player)

    HandlePlayerStepOnSkydiveTrigger(Player:player):void=
        Logger.Print("Verse: Player stepped on trigger that should teleport into a skydive")
        TeleportSkydive.Teleport(Player)
    <#
    # Runs when this device_script is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Log("Verse device stopped!")
    #>
