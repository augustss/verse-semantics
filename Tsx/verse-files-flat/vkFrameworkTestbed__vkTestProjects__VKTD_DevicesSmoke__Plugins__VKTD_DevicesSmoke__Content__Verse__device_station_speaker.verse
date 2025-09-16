
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation}
using { /UnrealEngine.com/Temporary/Diagnostics }

log_speaker_device:=class(log_channel){}

# This is a new Verse based class that is used with the ScriptDevice in UEFN
# 
# Getting started: <link to UEFN docs>
device_station_speaker := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_speaker_device}

    @editable; ButtonActivateSpeaker:button_device := button_device{}
    @editable; ButtonRegisterPlayer:button_device := button_device{}
    @editable; HUDMessageDevice:hud_message_device := hud_message_device{}
    @editable; SpeakerDevice:speaker_device := speaker_device{}

    var ActivateButtonEventSubscription:?cancellable := false
    var RegisterPlayerButtonEventSubscription:?cancellable := false
    var PlayerScriptReference:?player := false

    # Runs when this device_script is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Verse device started!")

        set ActivateButtonEventSubscription = option{ ButtonActivateSpeaker.InteractedWithEvent.Subscribe(HandleSpeakerActivateEnableButton) }
        set RegisterPlayerButtonEventSubscription = option{ ButtonRegisterPlayer.InteractedWithEvent.Subscribe(HandleSpeakerRegisterPlayerButton) }
    
    HandleSpeakerActivateEnableButton(Player:player):void=
        #ShowButtonEventSubscription?.Cancel() || true
        Logger.Print("Verse: Speaker has been activated")
        SpeakerDevice.Activate()
        set PlayerScriptReference = option { Player }
        HUDMessageDevice.Show(PlayerScriptReference)

    HandleSpeakerRegisterPlayerButton(Player:player):void=
        #HideButtonEventSubscription?.Cancel() || true
        Logger.Print("Verse: Speaker has registered Player")
        SpeakerDevice.RegisterPlayer(Player)
    <#
    # Runs when this device_script is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Log("Verse device stopped!")
    #>
