using { /Fortnite.com/Characters }
using { /Fortnite.com/Game }
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Simulation/Tags }

teamdeath_spawner := class(tag) {}



teamdeath_game_manager := class(creative_device):

        PlayerNameToMessage<localizes>(Agent:agent):message="{Agent}"
                                                                var PlayerNames : []message = array {}



                                                                var PlayersMap:[player]teamdeath_player = map{}
                                                                @editable
                                                                var Sentries : []sentry_device = array {}
                                                                var Team1Score:int = 0
                                                                var Team2Score:int = 0
                                                                @editable
                                                                var ElimsToEndGame:int = 3

                                                                @editable
                                                                EG: end_game_device = end_game_device {}
                                                                @editable
                                                                SM:score_manager_device = score_manager_device{}

    GetPlayerName():void=
                                                        # Random function to grab the players names. May use this as a kill feed for who killed who later on.
                                                                Players := GetPlayspace().GetPlayers()
                                                                for (ObtainedPlayer : Players):
                                                                    CurrentPlayerName : message = PlayerNameToMessage(ObtainedPlayer)
                                                                    set PlayerNames = PlayerNames + array{CurrentPlayerName}
                                                                    Print("Player names obtained")
    OnEnd<override>():void=
                                                                    Print ("Nexus OnEnd - teamdeath_gamemanager")
    OnBegin<override>()<suspends>:void=
                                                                    Print ("Nexus OnBegin - teamdeath_gamemanager")
                                                                GetPlayerName()

                                                                InitTestSentries()
                                                                SetUpTeamDeathSpawners()
                                                                GetPlayspace().PlayerRemovedEvent().Subscribe(OnPlayerRemoved)
                                                                # Print("It is: {SM.GetCurrentScore}")
    InitTestSentries():void=
                                                                Print("Sentry Initialized")
                                                                for (Sentry : Sentries):
                                                                    Sentry.EliminatedEvent.Subscribe(TestPlayerElim)

    TestPlayerElim(Agent : ?agent):void=
                                                                Print ("Sentry eliminated")
                                                                if (_Player := Agent?):
                                                                    UpdateScore()
    SetUpTeamDeathSpawners():void=
                                                                TeamDeathSpawners := GetCreativeObjectsWithTag(teamdeath_spawner{})

                                                                for (AllTeamDeathSpawners : TeamDeathSpawners, TeamDeathSpawner := player_spawner_device[AllTeamDeathSpawners]):
                                                                    TeamDeathSpawner.SpawnedEvent.Subscribe(OnPlayerSpawned)

    UpdateScore():void=
        # Here we update the team scores for the person who actually scored pertaining to their Team index
                                                                for(_Player : PlayersMap):
                                                                    Team:int = _Player.GetTeam()
                                                                    if(Team = 0):
                                                                        set Team1Score = SM.GetCurrentScore(_Player.MyAgentObj)
                                                                        Print("The T1 Score is {SM.GetCurrentScore(_Player.MyAgentObj)}")
                                                                    else:
                                                                        set Team2Score = SM.GetCurrentScore(_Player.MyAgentObj)
                                                                        Print("The T2 Score is {SM.GetCurrentScore(_Player.MyAgentObj)}")
                                                                    _Player.UpdateScore(Team1Score, Team2Score)
                                                                    CheckEndGame(_Player.MyAgentObj, Team)
    CheckEndGame(_Player : agent, Score: int):void=
        # We check to see if the Team1 or Team2 score is equal to or greater than the required elims to end the game
                                                                if (Team1Score >= ElimsToEndGame):
                                                                    Print ("Ending the game T1 Wins")
                                                                    EG.Activate(_Player)
                                                                else:
                                                                    if (Team2Score >=ElimsToEndGame):
                                                                        Print ("Ending the game T2 Wins")
                                                                        EG.Activate(_Player)

    OnPlayerSpawned(Agent:agent):void=
                                                                    if:
                                                                        PlayerObj := player[Agent]
                                                                    then:
                                                                        if(PlayerExists := PlayersMap[PlayerObj]):
                                                                            #do nothing, teamdeath_player already exists
                                                                        else:
                                                                            #setting the teamdeath_player to have an eliminated event to listen to
                                                                            if(FC:fort_character = Agent.GetFortCharacter[]):
                                                                                                                                                    FC.EliminatedEvent().Subscribe(OnPlayerEliminated)
                                                                            NewCustomPlayer:teamdeath_player = teamdeath_player{MyAgentObj := Agent}
                                                                            if(set PlayersMap[PlayerObj] = NewCustomPlayer):
                                                                                                                                                    NewCustomPlayer.InitPlayer()
                                                                                
                                                                                #find out what team the teamdeath_player is on
                                                                                TeamCollection:=GetPlayspace().GetTeamCollection()
                                                                                
                                                                                if(PlayerTeam := TeamCollection.GetTeam[Agent]):
                                                                                    TeamsArray:=TeamCollection.GetTeams()
                                                                                    for(TeamNumber->Team : TeamsArray):
                                                                                                                                                    if(PlayerTeam = Team):
                                                                                                                                                        NewCustomPlayer.SetTeam(TeamNumber)
                                                                    else:
                                                                        Print ("NexusError -  teamdeath_gamemanager.OnPlayerSpawned(Agent : agent) | Agent not successfully added")   
            UpdateScore()


    OnPlayerRemoved(_Player:player):void=
        if(RemoveCustomPlayer:teamdeath_player = PlayersMap[_Player]):
                                                                    #Remove the teamdeath_player from the players
                                                                    var TempAllPlayersMap : [player]teamdeath_player = map{}
                                                                    for (Key -> Value : PlayersMap, Key <> _Player):
                                                                        set TempAllPlayersMap = ConcatenateMaps(TempAllPlayersMap, map{Key => Value})
                                                                    set PlayersMap = TempAllPlayersMap


    OnPlayerEliminated(ElimResult:elimination_result):void=
        Print("A teamdeath_player was eliminated")
        EliminatedFC:fort_character = ElimResult.EliminatedCharacter
                                                                    if(FortCharacter := ElimResult.EliminatingCharacter?):
                                                                        if(FortCharacter = EliminatedFC):
                                                                            #this is a self elim
                                                                            
                                                                        else:#Who did the eliminating
                                                                            if(EliminatingAgent := FortCharacter.GetAgent[], EliminatingPlayer := player[EliminatingAgent]):
                                                                                if(EliminatingCustomPlayer : teamdeath_player = PlayersMap[EliminatingPlayer]):
                                                                                    SM.Activate(EliminatingAgent)
                                                                                    
                                                                    #Who got eliminated
                                                                    if(EliminatedAgent:agent = EliminatedFC.GetAgent[], _Player:player = player[EliminatedAgent]):
                                                                        if(EliminatedCustomPlayer: teamdeath_player = PlayersMap[_Player]):
                                                                            GetPlayerName()
                

                                                                    UpdateScore()