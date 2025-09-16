
using { /Fortnite.com/FortPlayerUtilities }
using { /Fortnite.com/Characters }
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Verse.org/Simulation/Tags }

kickplayer_trigger<public> := class(tag){}

kicker_manager:= class (creative_device):
    @editable
    var Pos : vector3 = vector3 {}
    @editable
    var Rot : rotation = rotation {}
    @editable
    Button: button_device = button_device {}
    
    OnEnd<override>():void=
        Print ("Nexus OnEnd - kicker_manager")

# Calling our triggers and subscribing the PlayerKick function to them.
# Also starting the respawn loop and subscribing a button to do that for us OnBegin
    OnBegin<override>()<suspends>:void=
        Print ("Nexus OnBegin - kicker_manager")

        FindTriggerKickers()
        Button.InteractedWithEvent.Subscribe(RespawnPlayer)
        LoopRespawn()


# Logic for kicking the player from the game using Trigger Devices
    FindTriggerKickers():void=
        AllTriggerKickers := GetCreativeObjectsWithTag (kickplayer_trigger{})

        for (TheTriggerKickers: AllTriggerKickers, TriggerKicker:= trigger_device[TheTriggerKickers]):
            TriggerKicker.TriggeredEvent.Subscribe(PlayerKick)
            
    PlayerKick(Agent : ?agent) : void =
        if(InPlayer := player[Agent?]):
            InPlayer.SendToLobby()

# Logic for respawning the player
    LoopRespawn()<suspends>:void=
        loop:
            Print("Player will respawn in 60 seconds")
            Sleep(60.0)
            Players := Self.GetPlayspace().GetPlayers()
            for(SomePlayer:Players):
                PlayerAgent:agent = SomePlayer
                RespawnPlayer(SomePlayer)
    RespawnPlayer(Agent : agent): void=
        if (Player := player[Agent]):
            Player.Respawn((Pos, Rot))
            Print ("Player successfully respawned")
            Print ("Respawned at Pos: X:{Pos.X}, Y:{Pos.Y}, Z:{Pos.Z}")
            Print ("Respawned at Rot: {Rot}")
        else:
            Print ("Nexus Error - kicker_manager.RespawnPlayer(Agent : agent) | Agent was not successfully passed in for the player")

    


    
    # Foo(InPlayer:?agent):void=
    #     if (Player:= InPlayer?):
    #         Player.Respawn((Pos, Rot))

    # AddNumbers (Inta : int, Intb : int):void =
    #     block:
    # AddNumbers (Inta: float, Intb : float):void =
    #     block: