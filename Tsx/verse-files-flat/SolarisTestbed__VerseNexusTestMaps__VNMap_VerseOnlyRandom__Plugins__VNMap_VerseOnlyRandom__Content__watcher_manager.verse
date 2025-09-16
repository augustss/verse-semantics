using { /Fortnite.com/Characters }
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Fortnite.com/Playspaces}

watcher_manager := class(creative_device):
    @editable
    Watcher: creative_prop = creative_prop{}
    @editable
    FollowButton: button_device = button_device{}

    var IsFollowing:logic = false

    OnEnd<override>():void=
        Print ("Nexus OnEnd - watcher_manager")

# Getting the players in the level and an agent reference to start watching with the Watcher
    OnBegin<override>()<suspends>:void=
        Print ("Nexus OnBegin - watcher_manager")
        Players := Self.GetPlayspace().GetPlayers()
            for(SomePlayer:Players):
                PlayerAgent:agent = SomePlayer
                FollowPlayer(PlayerAgent)
        FollowButton.InteractedWithEvent.Subscribe(FollowPlayer)

    # Depending on the state of the IsFollowing logic, we spawn MakeWatcherFollow on another thread
    FollowPlayer(Agent:agent): void=
            if(IsFollowing = false):
                set IsFollowing = true
                spawn:
                    MakeWatcherFollow(Agent)
            else:
                if (IsFollowing = true):
                    spawn:
                        MakeWatcherFollow(Agent)
                else:
                    set IsFollowing = false
    # Makes the watcher look at the agent
    MakeWatcherFollow(Agent:agent)<suspends>:void=
        Print ("Watcher is watching a player")
        loop:
            if (IsFollowing = true):
                if (PlayerCharacter := Agent.GetFortCharacter[]):

                    FollowWatcherLocation := Watcher.GetTransform().Translation
                    PlayerLocation := PlayerCharacter.GetTransform().Translation

                    if (LookDirection := (PlayerLocation - FollowWatcherLocation).MakeUnitVector[]):
                        # Left to Right
                        Yaw := RadiansToDegrees(ArcTan(LookDirection.Y, LookDirection.X))
                        # Up and down
                        Pitch := RadiansToDegrees(ArcTan(LookDirection.Z, Sqrt((LookDirection.X * LookDirection.X)+ (LookDirection.Y * LookDirection.Y))))
                        # Don't want it to roll so set to 0.0
                        Roll := 0.0

                        NewRotation := MakeRotationFromYawPitchRollDegrees(Yaw, Pitch, Roll)
                        Watcher.MoveTo(FollowWatcherLocation, NewRotation, 0.2)
                        # Print ("The Watcher is successfully watching a player")
            else:
                Print ("The Watcher has failed to watch a player")
                break

