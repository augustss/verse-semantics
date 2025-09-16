using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation}
using { /UnrealEngine.com/Temporary/Diagnostics }

log_item_granter_device:=class(log_channel){}

# This is a new Verse based class that is used with the ScriptDevice in UEFN
# 
# Getting started: <link to UEFN docs>
device_station_item_granter := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_item_granter_device}

    @editable; ButtonGrantDevice:button_device := button_device{}
    @editable; ItemGranterDevice:item_granter_device := item_granter_device{}

    var EnableButtonEventSubscription:?cancellable := false

    # Runs when this device_script is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Verse device started!")

        set EnableButtonEventSubscription = option{ ButtonGrantDevice.InteractedWithEvent.Subscribe(HandleGrantButton) }

    HandleGrantButton(Player:player):void=
        #ShowButtonEventSubscription?.Cancel() || true
        Logger.Print("Verse: Player Items Should Be Granted")
        ItemGranterDevice.GrantItem(Player)

    <#
    # Runs when this device_script is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Log("Verse device stopped!")
    #>
