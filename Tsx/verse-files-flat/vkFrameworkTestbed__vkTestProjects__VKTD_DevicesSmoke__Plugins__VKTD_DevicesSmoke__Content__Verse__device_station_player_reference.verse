
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

log_player_reference_device:=class(log_channel){}

# This is a new Verse based class that is used with the ScriptDevice in UEFN
# 
# Getting started: <link to UEFN docs>
device_station_player_reference := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_player_reference_device}

    @editable; PlayerReferenceDevice:player_reference_device = player_reference_device{}
    @editable; ButtonToActivatePlayerReference:button_device = button_device{}

    var ActivatePlayerEventSubscription:?cancellable := false 

    # Runs when this device_script is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Verse device started!")
        
        set ActivatePlayerEventSubscription = option { PlayerReferenceDevice.ActivatedEvent.Subscribe(HandleButtonActivate) }

    HandleButtonActivate(Player:player):void=
        Logger.Print("Verse - Player Reference has been activated")
        PlayerReferenceDevice.Activate()
    <#
    # Runs when this device_script is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Log("Verse device stopped!")
    #>
