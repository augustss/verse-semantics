
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

log_device_station_capture_item_spawner:=class(log_channel){}

# This is a Verse-authored creative device that can be placed in a level
# 
# Getting started:
#   https://www.epicgames.com/fortnite/en-US/creative/docs/uefn/programming-with-verse-in-unreal-editor-for-fortnite
#
device_station_capture_item_spawner := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_device_station_capture_item_spawner}

    @editable; SwitchVerseToggleCaptureItemSpawner:switch_device = switch_device{}
    @editable; CaptureItemSpawnerDevice:capture_item_spawner_device = capture_item_spawner_device{}

    var SwitchVerseTurnedOnEventSubscription:?cancellable := false
    var SwitchVerseTurnedOffEventSubscription:?cancellable := false

    # Runs when this creative_device is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Verse device started!")

        set SwitchVerseTurnedOffEventSubscription = option { SwitchVerseToggleCaptureItemSpawner.TurnedOffEvent.Subscribe(HandleVerseSwitchTurnedOn) }
        
        set SwitchVerseTurnedOnEventSubscription = option { SwitchVerseToggleCaptureItemSpawner.TurnedOnEvent.Subscribe(HandleVerseSwitchTurnedOff) }

    HandleVerseSwitchTurnedOn():void=
        Logger.Print("Verse: Capture Item Spawner Should Be Gettting Turned on through Verse")
        CaptureItemSpawnerDevice.Enable()
    
    HandleVerseSwitchTurnedOff():void=
        Logger.Print("Verse:Capture Item Spawner Should Be Gettting Destroyed through Verse")
        CaptureItemSpawnerDevice.Disable()

    <#
    # Runs when this creative_device is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Logger.Print("Verse device stopped!")
    #>
