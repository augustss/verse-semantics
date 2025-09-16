
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation}
using { /UnrealEngine.com/Temporary/Diagnostics }

log_damage_vol_device:=class(log_channel){}

# This is a new Verse based class that is used with the ScriptDevice in UEFN
# 
# Getting started: <link to UEFN docs>
device_station_damage_volume := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_damage_vol_device}


    @editable; ButtonEnableDevice:button_device := button_device{}
    @editable; ButtonDisableDevice:button_device := button_device{}
    @editable; DamageVolumeDevice:damage_volume_device := damage_volume_device{}

    var EnableButtonEventSubscription:?cancellable := false
    var DisableButtonEventSubscription:?cancellable := false

    # Runs when this device_script is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Verse device started!")

        set EnableButtonEventSubscription = option{ ButtonEnableDevice.InteractedWithEvent.Subscribe(HandleEnableButton) }
        set DisableButtonEventSubscription = option{ ButtonDisableDevice.InteractedWithEvent.Subscribe(HandleDisableButton) }

    HandleEnableButton(Player:player):void=
        #ShowButtonEventSubscription?.Cancel() || true
        Logger.Print("Verse: Damage Vol Should be Enabled")
        DamageVolumeDevice.Enable()

    HandleDisableButton(Player:player):void=
        #HideButtonEventSubscription?.Cancel() || true
        Logger.Print("Verse: Damage Vol Should be Disabled")
        DamageVolumeDevice.Disable()


    <#
    # Runs when this device_script is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Log("Verse device stopped!")
    #>
