
using { /Fortnite.com/Devices }
using { /Verse.org/Native }
using { /Verse.org/Simulation}
using { /UnrealEngine.com/Temporary/Diagnostics }

log_new_device:=class(log_channel){}

# This is a new Verse based class that is used with the ScriptDevice in UEFN
# This script example is a take on Parkour where the objective is to gather 4 bananas before the timer expires
# Getting started: <link to UEFN docs>
parkour_race_script := class<concrete>(creative_device):
    Logger:log = log{Channel := log_new_device}

    # Device reference to the player
    @editable
    PlayerSpawnDevice:player_spawner_device := player_spawner_device{}
    # End Game Device for a Victory
    @editable
    EndGameVictoryDevice:end_game_device := end_game_device{}
    # End Game Device for a Defeat
    @editable
    EndGameDefeatDevice:end_game_device := end_game_device{}
    # Timer for this game mode
    @editable
    TimerDevice:timer_device := timer_device{}
    # Hud message feedback for the player when they get a banana
    @editable
    HUDMessageBanana:hud_message_device := hud_message_device{}

    # These are references to the 4 bananas the player will collect. They come from 4 Item Spawner Devices
    @editable
    FirstBanana:item_spawner_device := item_spawner_device{}
    @editable
    SecondBanana:item_spawner_device := item_spawner_device{}
    @editable
    ThirdBanana:item_spawner_device := item_spawner_device{}
    @editable
    FourthBanana:item_spawner_device := item_spawner_device{}
        
    # Syntax for exposing an array. Not currently supported in Editor
    # @editable
    # var ItemSpawners: []item_spawner_device := array{}
    
    #
    # Subscriptions to different events we can bind to in script
    #
    
    # Subscription to when we pick up an item
    var ItemPickedUpSubscription:?cancellable := false
    # Subscription to when the player spawns
    var PlayerSpawnedSubscripion:?cancellable := false
    # Subscription to when the timer expires. We only care about when it fails
    var TimerExpiredSubscription:?cancellable := false    

    # Declaration for and integer that represents the number of bananas we've collected so far
    var BananasCollected:int := 0
    # An array of all the Item Spawners we have. This is used to demonstrate using arrays
    var BananaItemSpawners:[]item_spawner_device := array{}
    
    # Runs when this device_script is started in a running game
    OnBegin<override>()<suspends>:void=
        # You can output to the log like this to determine what your script is up to
        Logger.Print("Parkour Race Script Started!")

        # Subscribing to the PlayerSpawnedEvent.
        # When the player spawns, the function "HandlePlayerSpawned" is called.
        # The "Subscribe" function returns a handle. This Handle is what PlayerSpawnedSubscription is set to. You can use the handle to unsubscribe
        set PlayerSpawnedSubscripion = option{ PlayerSpawnDevice.PlayerSpawnedEvent.Subscribe(HandlePlayerSpawned) }
        # Same for the Timer. We subscribe to when it expires
        set TimerExpiredSubscription = option{ TimerDevice.FailureEvent.Subscribe(HandleTimerExpired) }

        # We add all 4 Banana Item Spawners to our array. Prior to this our array is empty
        set BananaItemSpawners += array{ FirstBanana, SecondBanana, ThirdBanana, FourthBanana }

        # We go through our array of BananaItemSpawners and for each Item Spawner, we subscribe to the ItemPickupEvent.
        # We do this for each Item Spawner and we don't save the handle like we did for the PlayerSpawnedSubscription and TimerExpiredSubscription above
        # You don't have to save the handle, but without it, you won't be able to cancel the subscription so it will fire every time an item is picked up. 
        for ( BananaSpawner : BananaItemSpawners):
            BananaSpawner.ItemPickedUpEvent.Subscribe(HandleBananaPickedUp)
        
        # An alternate way to do the for loop above that uses an integer to walk through the array
        #   for (j:int := 0..BananaItemSpawners.Length - 1):
        #     if (Banana := BananaItemSpawners[j]):
        #         Banana.ItemPickedUpEvent.Subscribe(HandleBananaPickedUp)
    
    # A function that is called when the Player Spawns from the Player Spawn Pad
    HandlePlayerSpawned(Player:player):void=
        # Log that the player Spawned
        Logger.Print("Player Spawned!")
        # Tell our Timer Device to Begin
        TimerDevice.Begin(Player)
    
    # Function that is called when a banana item is picked up from the Item Spawners
    HandleBananaPickedUp(Player:player):void=
        # Increment the number of bananas collected
        set BananasCollected = BananasCollected + 1
        # This is how you can output the number of bananas to the log. Useful for debugging        
        Logger.Print("Number of bananas picked up: {BananasCollected}")

        # An if statement. Check to see if a condition is true, run some code. If it isn't true, run some other code.
        if (BananasCollected >= 4):
            # In this case, we check to see if we've collected 4 (or more) bananas. If so, we have won, activate the End Game Device
            EndGameVictoryDevice.Activate(Player)
        else:
            # This code runs if the number of bananas is less than 4. Show a Hud Message to spur the player on
            HUDMessageBanana.Show()

            # Then we get the next Item Spawner in our array we set up
            # We do this by "indexing" into the array. It is inside the "if" statement to ensure that NextBananaSpawner is referenced correctly
            if (NextBananaSpawner := BananaItemSpawners[BananasCollected]):
                # If we got the next item spawner, call SpawnItem which will activate the next banana to get
                NextBananaSpawner.SpawnItem(Player)

    # Function that is called when the timer expires
    HandleTimerExpired(Player:player):void=
        # Log to Output
        Logger.Print("Timer Ended")
        # Activate the End Game Device that notifies the player's defeat!
        EndGameDefeatDevice.Activate(Player)

    # Runs when this device_script is stopped or the game ends
    OnEnd<override>():void=
        Logger.Print("Verse device stopped!")
