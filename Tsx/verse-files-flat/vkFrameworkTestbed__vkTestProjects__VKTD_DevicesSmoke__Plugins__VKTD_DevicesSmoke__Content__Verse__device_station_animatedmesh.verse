
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation}
using { /UnrealEngine.com/Temporary/Diagnostics }

log_device_station_animatedmesh:=class(log_channel){}

# This is a Verse-authored creative device that can be placed in a level
# 
# Getting started:
#   https://www.epicgames.com/fortnite/en-US/creative/docs/uefn/programming-with-verse-in-unreal-editor-for-fortnite
#
device_station_animatedmesh := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_device_station_animatedmesh}

    @editable; ButtonPlayAnim:button_device := button_device{}
    @editable; ButtonPauseAnim:button_device := button_device{}
    @editable; ButtonReverseAnim:button_device := button_device{}
    @editable; AnimatedMeshDevice:animated_mesh_device := animated_mesh_device{}

    var PlayButtonEventSubscription:?cancellable := false
    var PauseButtonEventSubscription:?cancellable := false
    var ReverseButtonEventSubscription:?cancellable := false

    # Runs when this creative_device is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Verse Animated Mesh device started!")

        set PlayButtonEventSubscription = option{ ButtonPlayAnim.InteractedWithEvent.Subscribe(HandlePlayButton) }
        set PauseButtonEventSubscription = option{ ButtonPauseAnim.InteractedWithEvent.Subscribe(HandlePauseButton) }
        set ReverseButtonEventSubscription = option{ ButtonReverseAnim.InteractedWithEvent.Subscribe(HandleReverseButton) }

    HandlePlayButton(Player:player):void=
        Logger.Print("Verse: Animation should play")
        AnimatedMeshDevice.PlayAnimation()

    HandlePauseButton(Player:player):void=
        Logger.Print("Verse: Animation should Pause")
        AnimatedMeshDevice.PauseAnimation()

    HandleReverseButton(Player:player):void=
        Logger.Print("Verse: Animation should play in reverse")
        AnimatedMeshDevice.PlayReverseAnimation()
    <#
    # Runs when this creative_device is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Logger.Print("Verse device stopped!")
    #>
