
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation}
using { /UnrealEngine.com/Temporary/Diagnostics }

log_item_spawner_device:=class(log_channel){}

# This is a new Verse based class that is used with the ScriptDevice in UEFN
# 
# Getting started: <link to UEFN docs>
device_station_item_spawner := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_item_spawner_device}

    @editable; ButtonSpawnItemDevice:button_device := button_device{}
    @editable; ButtonCycleItemDevice:button_device := button_device{}
    @editable; ItemSpawnerDevice:item_spawner_device := item_spawner_device{}
    @editable; ItemSpawnerSwitch:switch_device := switch_device{} 

    var SpawnItemButtonEventSubscription:?cancellable := false
    var CycleItemButtonEventSubscription:?cancellable := false
    var SwitchOnEventSubscription:?cancellable := false
    var SwitchOffEventSubscription:?cancellable := false
    var ItemPickedUpEventSubscription:?cancellable := false

    # Runs when this device_script is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Verse device started!")

        set SpawnItemButtonEventSubscription = option{ ButtonSpawnItemDevice.InteractedWithEvent.Subscribe(HandleSpawnButton) }
        set CycleItemButtonEventSubscription = option{ ButtonCycleItemDevice.InteractedWithEvent.Subscribe(HandleCycleItemButton) }
        set SwitchOffEventSubscription = option{ ItemSpawnerSwitch.TurnedOffEvent.Subscribe(HandleSwitchOff) }
        set SwitchOnEventSubscription = option{ ItemSpawnerSwitch.TurnedOnEvent.Subscribe(HandleSwitchOn) }
        set ItemPickedUpEventSubscription = option{ ItemSpawnerDevice.ItemPickedUpEvent.Subscribe(HandleItemPickedUp) }

    HandleSpawnButton(Player:player):void=
        #ShowButtonEventSubscription?.Cancel() || true
        Logger.Print("Verse: Item Spawner Device Spawned Item")
        ItemSpawnerDevice.SpawnItem(Player)

    HandleCycleItemButton(Player:player):void=
        #HideButtonEventSubscription?.Cancel() || true
        Logger.Print("Verse: Item Spawner Cycled Item")
        ItemSpawnerDevice.CycleToNextItem()

    HandleSwitchOff():void=
        #ShowButtonEventSubscription?.Cancel() || true
        Logger.Print("Verse: Item Spawner Disabled")
        ItemSpawnerDevice.Disable()
    
    HandleSwitchOn():void=
        #HideButtonEventSubscription?.Cancel() || true
        Logger.Print("Verse: Item Spawner Enabled")
        ItemSpawnerDevice.Enable()
    
    HandleItemPickedUp(Player:player):void=
        #HideButtonEventSubscription?.Cancel() || true
        Logger.Print("Verse: Item Spawner Item Pickedup By Player")

    <#
    # Runs when this device_script is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Log("Verse device stopped!")
    #>
