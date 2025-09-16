
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation}
using { /UnrealEngine.com/Temporary/Diagnostics }

log_endgame_device:=class(log_channel){}

# This is a new Verse based class that is used with the ScriptDevice in UEFN
# 
# Getting started: <link to UEFN docs>
device_station_endgame := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_endgame_device}

    @editable; ButtonActivateEndGame:button_device := button_device{}
    @editable; EndGameDevice:end_game_device := end_game_device{}

    var ActivateButtonEventSubscription:?cancellable := false

    # Runs when this device_script is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Verse device started!")

        set ActivateButtonEventSubscription = option{ ButtonActivateEndGame.InteractedWithEvent.Subscribe(HandleActivateEndGameButton) }

    HandleActivateEndGameButton(Player:player):void=
        Logger.Print("Verse: End Game has been triggered")
        EndGameDevice.Activate(Player)

    <#
    # Runs when this device_script is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Log("Verse device stopped!")
    #>
