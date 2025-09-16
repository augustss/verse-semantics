
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation}
using { /UnrealEngine.com/Temporary/Diagnostics }

log_device_station_wildlife_spawner:=class(log_channel){}

# This is a Verse-authored creative device that can be placed in a level
# 
# Getting started:
#   https://www.epicgames.com/fortnite/en-US/creative/docs/uefn/programming-with-verse-in-unreal-editor-for-fortnite
#
device_station_wildlife_spawner := class<concrete>(creative_device):
    Logger:log = log{Channel:=log_device_station_wildlife_spawner}

    @editable; ButtonSpawnPred:button_device := button_device{}
    @editable; ButtonDeSpawnPred:button_device := button_device{}
    @editable; ButtonTamePred:button_device := button_device{}
    @editable; ButtonUntamePred:button_device := button_device{}
    @editable; PredWildlifeSpawner:wildlife_spawner_device := wildlife_spawner_device{}
    @editable; PreyWildlifeSpawner:wildlife_spawner_device := wildlife_spawner_device{}

    var SpawnPredButtonEventSubscription:?cancellable := false
    var DespawnPredButtonEventSubscription:?cancellable := false
    var TamePredButtonEventSubscription:?cancellable := false
    var UntamePredButtonEventSubscription:?cancellable := false
    var SpawnedEventSubscription:?cancellable := false
    var DamagedEventSubscription:?cancellable := false
    var DismountedEventSubscription:?cancellable := false
    var EliminatedByPredEventSub:?cancellable := false

    # Runs when this creative_device is started in a running game
    OnBegin<override>()<suspends>:void=
        # Replace this with your code
        Logger.Print("Verse device started!")

        set SpawnPredButtonEventSubscription = option{ ButtonSpawnPred.InteractedWithEvent.Subscribe(HandleSpawnPredButton) }
        set DespawnPredButtonEventSubscription = option{ ButtonDeSpawnPred.InteractedWithEvent.Subscribe(HandleDespawnPredButton) }
        set TamePredButtonEventSubscription = option{ ButtonTamePred.InteractedWithEvent.Subscribe(HandleTamePredButton) }
        set UntamePredButtonEventSubscription = option{ ButtonUntamePred.InteractedWithEvent.Subscribe(HandleUntamePredButton) }
        set EliminatedByPredEventSub = option{PreyWildlifeSpawner.EliminatedByPredatorEvent.Subscribe(HandleEatenByPredator)}
    
    HandleSpawnPredButton(Player:player):void=
        Logger.Print("Wildlife Spawner - Predator - Should Spawn")
        PredWildlifeSpawner.Spawn()

    HandleDespawnPredButton(Player:player):void=
        Logger.Print("Wildlife Spawner - Predator - Should Despawn")
        PredWildlifeSpawner.Despawn(Player)

    HandleTamePredButton(Player:player):void=
        Logger.Print("Wildlife Spawner - Predator - Should Now Be Tamed")
        PredWildlifeSpawner.Tame(Player)

    HandleUntamePredButton(Player:player):void=
        Logger.Print("Wildlife Spawner - Predator - Should become Untamed")
        PredWildlifeSpawner.UntameFromPlayer(Player)

    HandleEatenByPredator():void=
        Logger.Print("Prey Wildlife Spawner - Prey - Has been eliminated by predator")
        
        

    <#
    # Runs when this creative_device is stopped or the game ends
    OnEnd<override>():void=
        # Uncomment this function, then replace this with your code
        Logger.Print("Verse device stopped!")
    #>
