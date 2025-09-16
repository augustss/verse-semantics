
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation}
using { /UnrealEngine.com/Temporary/Diagnostics }

log_device_station_mutator_zone:=class(log_channel){}

# This is a Verse-authored creative device that can be placed in a level
# 
# Getting started:
#   https://www.epicgames.com/fortnite/en-US/creative/docs/uefn/programming-with-verse-in-unreal-editor-for-fortnite
#
device_station_mutator_zone := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_device_station_mutator_zone}

    @editable; ButtonEnableDevice:button_device := button_device{}
    @editable; ButtonDisableDevice:button_device := button_device{}
    @editable; MutatorZoneDevice:mutator_zone_device := mutator_zone_device{}

    var EnableButtonEventSubscription:?cancellable := false
    var DisableButtonEventSubscription:?cancellable := false

    # Runs when this creative_device is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Mutator Zone Station device started!")

        set EnableButtonEventSubscription = option{ ButtonEnableDevice.InteractedWithEvent.Subscribe(HandleEnableButton) }
        set DisableButtonEventSubscription = option{ ButtonDisableDevice.InteractedWithEvent.Subscribe(HandleDisableButton) }

    HandleEnableButton(Player:player):void=
        Logger.Print("Verse: Mutator Zone Should be Enabled")
        MutatorZoneDevice.Enable()

    HandleDisableButton(Player:player):void=
        Logger.Print("Verse: Mutator Zone Should be Disabled")
        MutatorZoneDevice.Disable()

    <#
    # Runs when this creative_device is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Logger.Print("Verse device stopped!")
    #>
