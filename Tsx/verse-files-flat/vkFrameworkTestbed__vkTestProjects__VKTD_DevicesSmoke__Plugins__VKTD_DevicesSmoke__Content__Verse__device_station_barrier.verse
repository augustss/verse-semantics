
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation}
using { /UnrealEngine.com/Temporary/Diagnostics }

log_barrier_device:=class(log_channel){}

# This is a new Verse based class that is used with the ScriptDevice in UEFN
# 
# Getting started: <link to UEFN docs>
device_station_barrier := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_barrier_device}

    
    @editable; ButtonEnableBarrier:button_device := button_device{}
    @editable; ButtonDisableBarrier:button_device := button_device{}
    @editable; BarrierDevice:barrier_device := barrier_device{}

    var EnableButtonEventSubscription:?cancellable := false
    var DisableButtonEventSubscription:?cancellable := false

    # Runs when this device_script is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Verse device started!")
        set EnableButtonEventSubscription = option{ ButtonEnableBarrier.InteractedWithEvent.Subscribe(HandleBarrierEnableButton) }
        set DisableButtonEventSubscription = option{ ButtonDisableBarrier.InteractedWithEvent.Subscribe(HandleBarrierDisableButton) }


    HandleBarrierEnableButton(Player:player):void=
        #ShowButtonEventSubscription?.Cancel() || true
        Logger.Print("Verse: Barrier Should be Enabled")
        BarrierDevice.Enable()

    HandleBarrierDisableButton(Player:player):void=
        #HideButtonEventSubscription?.Cancel() || true
        Logger.Print("Verse: Barrier Should be Disabled")
        BarrierDevice.Disable()

    <#
    # Runs when this device_script is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Log("Verse device stopped!")
    #>
