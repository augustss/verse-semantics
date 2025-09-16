
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

log_device_station_down_but_not_out:=class(log_channel){}

# This is a Verse-authored creative device that can be placed in a level
# 
# Getting started:
#   https://www.epicgames.com/fortnite/en-US/creative/docs/uefn/programming-with-verse-in-unreal-editor-for-fortnite
#
device_station_down_but_not_out := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_device_station_down_but_not_out}

    @editable; ButtonEnableDevice:button_device := button_device{}
    @editable; ButtonDisableDevice:button_device := button_device{}
    @editable; ButtonDownPlayer:button_device := button_device{}
    @editable; DownButNotOut:dbno_device := dbno_device{}

    var EnableButtonEventSubscription:?cancellable := false
    var DisableButtonEventSubscription:?cancellable := false
    var DownedButtonEventSubscription:?cancellable := false

    var TimesEnabled: int = 0

    # Runs when this creative_device is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Down But Not Out Device Verse Station started!")

        set EnableButtonEventSubscription = option{ ButtonEnableDevice.InteractedWithEvent.Subscribe(HandleEnableDeviceButton) }
        set DisableButtonEventSubscription = option{ ButtonDisableDevice.InteractedWithEvent.Subscribe(HandleDisableDeviceButton) }
        set DownedButtonEventSubscription = option{ ButtonDownPlayer.InteractedWithEvent.Subscribe(HandleDownPlayerButton) }
    HandleEnableDeviceButton(Player:player):void=
        set TimesEnabled = TimesEnabled + 1
        Logger.Print("Verse: DownButNotOut should be Enabled / Count = {TimesEnabled} ")
        DownButNotOut.Enable()
    
    HandleDisableDeviceButton(Player:player):void=
        Logger.Print("Verse: DownButNotOut should be Disbled")
        DownButNotOut.Disable()

    HandleDownPlayerButton(Player:player):void=
        Logger.Print("Verse: DownButNotOut should trigger player down")
        DownButNotOut.DownPlayer(Player)


    <#
    # Runs when this creative_device is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Logger.Print("Verse device stopped!")
    #>
