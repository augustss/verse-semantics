using { /Fortnite.com/Game }
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Fortnite.com/Characters }
# See https://dev.epicgames.com/documentation/en-us/uefn/create-your-own-device-in-verse for how to create a verse device.

# A Verse-authored creative device that can be placed in a level
using { /Verse.org/Simulation/Tags }

boulder_tag := class(tag){}
explosives_tag := class(tag){}

obstacle_device<public> := class<concrete>(creative_device):

    RoundName<protected> : string = "Obstacle"
    RoundXP<public> : int = 150
    RoundTime<protected> : float = 15.0

    Notify<public>():void=
        Print("Notification received")

    BeginRound<public>()<suspends>:void=
        Print("Round began received")

    EndRound<public>()<suspends>:void=
        Print("Round began received")

    GetRoundTime<public>():float=RoundTime

    OnBegin<override>()<suspends>:void=
        Print("Parent Obstacle Device reached")

obstacle_manager := struct<concrete>:
    @editable
    Obstacles:[]obstacle_device = array{}

game_manager := class(creative_device):

    @editable   
    RoundSettingsDevice:round_settings_device=round_settings_device{}

    @editable
    EndGameDevice:end_game_device=end_game_device{}

    @editable
    ObstacleManager : obstacle_manager=obstacle_manager{}

    var Players<public>:[]player = array{}

    var PlayersAsAgent:[]agent = array{}

    var EliminatedPlayers<public>:[]agent = array{}

    @editable
    ScoreManager:score_manager_device=score_manager_device{}

    @editable
    HUDDevice:hud_device = hud_device{}

    @editable EmergencyAlertTime<internal> : float = 5.0

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print("GCTEST : Game Start")
        set Players = Self.GetPlayspace().GetPlayers()
        

        for(Player:Players, FortChar:=Player.GetFortCharacter[], PlayerAsAgent:= agent(Player)):

            FortChar.EliminatedEvent().Subscribe(OnPlayerEliminated)

            set PlayersAsAgent += array{PlayerAsAgent}


        Sleep(1.0)
        StartRounds()
        RoundTimer()

    OnEnd<override>():void=
        Print("GCTEST : Game End")

    StartRounds()<suspends>:void=
        for(Obstacle : ObstacleManager.Obstacles):
            StartRoundTask := spawn{Obstacle.BeginRound()}



    var RoundTime:int = 0

    @editable
    RoundEndTime:int = 20

    RoundTimer()<suspends>:void=
        block:
            loop:
                Sleep(1.0)
                set RoundTime += 1 
                CheckNearEndTime(RoundTime)

                for(PlayerAgent:PlayersAsAgent, HasPlayerBeenEliminated(PlayerAgent) = false):
                    ScoreManager.Activate(PlayerAgent)


                if(RoundTime>RoundEndTime, Player1:=Players[0]):
                    RoundSettingsDevice.EndRound(Player1)


    OnPlayerEliminated(EliminationResult:elimination_result):void=

        Print("On Player Eliminated worked")
        if(set EliminatedPlayers += array{EliminationChar := (EliminationResult.EliminatedCharacter.GetAgent[])}):
            Print("Eliminated players = {EliminatedPlayers.Length}")

    HasPlayerBeenEliminated(Agent:agent)<transacts>:logic=
        
        for(DeadPlayer:EliminatedPlayers, Agent = DeadPlayer) {return true}

        return false
            
    CheckNearEndTime(TimeInt:int):void=

        TimeIntAsFloat := TimeInt * 1.0

        # Check if it's time to add the timer
        RoundTimeLeftInt:=RoundEndTime-TimeInt
        if(RoundTimeLeftInt*1.0=EmergencyAlertTime)
        {
        for(Player:PlayersAsAgent)
        {
        HUDDevice.AddPlayerUI(Player)
        }
        }

        # Update timer to descend
        if(RoundTimeLeftInt*1.0 <= EmergencyAlertTime)
        {
        HUDDevice.UpdateTimerSlider(RoundEndTime *1.0 - (TimeInt*1.0))
        }

        # Remove timer at the end
        if(TimeIntAsFloat <= 0.0)
        {

        for(Player:PlayersAsAgent)
        {
        HUDDevice.RemovePlayerUI(Player)
        }
        
        }

        