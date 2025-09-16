
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

log_device_station_cinematic_sequence:=class(log_channel){}

# This is a Verse-authored creative device that can be placed in a level
# 
# Getting started:
#   https://www.epicgames.com/fortnite/en-US/creative/docs/uefn/programming-with-verse-in-unreal-editor-for-fortnite
#
device_station_cinematic_sequence := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_device_station_cinematic_sequence}

    @editable; ButtonPlay:button_device := button_device{}
    @editable; CinematicSeqeuenceIntro:cinematic_sequence_device = cinematic_sequence_device{}

    var PlayButtonEventSubscription:?cancellable := false

    # Runs when this creative_device is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Verse device started!")

        set PlayButtonEventSubscription = option{ ButtonPlay.InteractedWithEvent.Subscribe(HandlePlaySeqButton) }

    HandlePlaySeqButton(Player:player):void=
        Logger.Print("Cinematic Sequence Level Intro Should Be Playing")
        CinematicSeqeuenceIntro.Play()

    <#
    # Runs when this creative_device is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Logger.Print("Verse device stopped!")
    #>
