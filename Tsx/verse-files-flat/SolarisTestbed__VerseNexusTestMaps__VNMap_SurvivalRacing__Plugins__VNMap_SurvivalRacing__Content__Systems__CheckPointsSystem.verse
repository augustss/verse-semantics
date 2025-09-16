using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/SpatialMath }

using { Utils }
using { Entities }


CheckPointsSystem<public> := class:

    CustomLogger:Logger = Logger{}

    Player<public>:PlayerWrapper
    
    AllCheckPoints<public>:[]race_checkpoint_device = array{}
    PassedCheckPoints:[]race_checkpoint_device = array{}

    var LastCheckPoint<private>:?race_checkpoint_device = false

    FarDistanceReset<public>:float

    var IsPlayerFarFromTrack<private>:logic = false


    Init<public>():void=
        CustomLogger.InfoLog.Print("Check Poins System Initing")
        MakeSubscriptions()

        spawn{ PlayerFarCheckPointWatcher() }


    PlayerFarCheckPointWatcher()<suspends>:void=
        loop:
            Sleep(0.1)
            if (Distance(Player.CharacterTransform.Translation, LastCheckPoint?.GetTransform().Translation) > FarDistanceReset):
                Print("Player is Far Far Away")
                set IsPlayerFarFromTrack = true
                ResetPlayerInLastCheckPoint()


    MakeSubscriptions():void=
        Print("Make subscriptions in Check Point System")
        for(CP : AllCheckPoints):
            CP.CheckpointCompletedEvent.SubscribeAgentWithExtra(OnPlayerPassingCheckPointSubscribe, CP)


    OnPlayerPassingCheckPointSubscribe(InAgent:agent, InPassedCP:race_checkpoint_device):void=
        Print("Player Passed Checkpoint")
        set LastCheckPoint = option{InPassedCP}

    
    ResetPlayerInLastCheckPoint():void=
        Print("Reset Player in Last Check Point")
        # set 
        if:
            LP := LastCheckPoint?
            Player.FortCharacter.TeleportTo[LP.GetTransform().Translation, LP.GetTransform().Rotation]
        then:
            Print("Player Returned on Last Check Point")
        else:
            Print("Player Returning on Last Check Point FAILED")


 


    
