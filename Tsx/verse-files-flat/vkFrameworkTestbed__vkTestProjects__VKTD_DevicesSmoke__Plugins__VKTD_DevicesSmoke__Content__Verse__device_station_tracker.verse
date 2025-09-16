
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation}
using { /UnrealEngine.com/Temporary/Diagnostics }

log_new_device:=class(log_channel){}

# This is a new Verse based class that is used with the ScriptDevice in UEFN
# 
# Getting started: <link to UEFN docs>
device_station_tracker := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_new_device}

    @editable; ButtonCompleteDevice:button_device := button_device{}
    @editable; ButtonResetDevice:button_device := button_device{}
    @editable; TrackerDevice:tracker_device := tracker_device{}

    var EnableButtonEventSubscription:?cancellable := false
    var DisableButtonEventSubscription:?cancellable := false

    # Runs when this device_script is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Verse device started!")

        set EnableButtonEventSubscription = option{ ButtonCompleteDevice.InteractedWithEvent.Subscribe(HandleCompleteButton) }
        set DisableButtonEventSubscription = option{ ButtonResetDevice.InteractedWithEvent.Subscribe(HandleResetButton) }

    HandleCompleteButton(Player:player):void=
        #ShowButtonEventSubscription?.Cancel() || true
        Logger.Print("Verse: Tracker should be completed")
        TrackerDevice.Complete(Player)

    HandleResetButton(Player:player):void=
        #HideButtonEventSubscription?.Cancel() || true
        Logger.Print("Verse: Tracker Progress should reset")
        TrackerDevice.ResetProgress(Player)
    <#
    # Runs when this device_script is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Log("Verse device stopped!")
    #>
