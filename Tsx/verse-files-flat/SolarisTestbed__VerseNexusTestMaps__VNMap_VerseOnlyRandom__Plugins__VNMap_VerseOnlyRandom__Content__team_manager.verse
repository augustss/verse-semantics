using { /Fortnite.com/Characters }
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

# A Verse-authored creative device that can be placed in a level
team_manager := class(creative_device):
    @editable
    MyNPCSpawnerDevice:npc_spawner_device = npc_spawner_device{}

    var Agents : []agent = array{}

    # Runs when the device is started in a running game
    OnBegin<override>()<suspends>:void=
        Print ("Nexus OnBegin - team_manager")

        for (Player : GetPlayspace().GetPlayers()):
            OnAgentSpawned(Player)

        GetPlayspace().PlayerAddedEvent().Subscribe(OnAgentSpawned)
        MyNPCSpawnerDevice.SpawnedEvent.Subscribe(OnAgentSpawned)

        # Make an NPC!
        MyNPCSpawnerDevice.Spawn()

    OnEnd<override>():void=
        Print ("Nexus OnEnd - team_manager")

    OnAgentSpawned(Agent:agent):void=
        TeamCollection := GetPlayspace().GetTeamCollection()
        Teams := TeamCollection.GetTeams()
        
        if:
            Team := TeamCollection.GetTeam[Agent]
        then:
            # Find a different team to assign the agent to.
            OtherTeams := for(OtherTeam : Teams; OtherTeam <> Team):
                OtherTeam

            # This test fails without AutoRTFM enabled (AddToTeam is not safe transactionally).
            # Re-enable when AutoRTFM is the only supported path #jira SOL-5997
            if:
                false?
            then:
                if:
                    OtherTeam := OtherTeams[0]
                    TeamCollection.AddToTeam[Agent, OtherTeam]
                    # Make it fail once!
                    false?
                then:
                    Err("Unreachable")
                else:
                    if:
                        Team <> TeamCollection.GetTeam[Agent]
                    then:
                        Print("Nexus Error - team_manager.OnAgentSpawned(Agent : agent) | Adding to a team had non-transactional side effects")
                        Err("Adding to a team had non-transactional side effects")

            if:
                OtherTeam := OtherTeams[0]
                TeamCollection.AddToTeam[Agent, OtherTeam]
            then:
                if:
                    OtherTeam <> TeamCollection.GetTeam[Agent]
                then:
                    Print("Nexus Error - team_manager.OnAgentSpawned(Agent : agent) | AddToTeam failed when it reported a success (GetTeam did not match)")
                    Err("AddToTeam failed when it reported it had succeeded (GetTeam did not match).")

                if:
                    not TeamCollection.IsOnTeam[Agent, OtherTeam]
                then:
                    Print("Nexus Error - team_manager.OnAgentSpawned(Agent : agent) | AddToTeam failed when it reported a success (IsOnTeam failed)")
                    Err("AddToTeam failed when it reported it had succeeded (IsOnTeam failed).")

                var Found :int= 0

                for(OtherAgent : TeamCollection.GetAgents[OtherTeam]; OtherAgent = Agent):
                    set Found += 1

                if:
                    Found <> 1
                then:
                    Print("Nexus Error - team_manager.OnAgentSpawned(Agent : agent) | AddToTeam failed when it reported a success (GetAgents did not return our agent)")
                    Err("AddToTeam failed when it reported it had succeeded (GetAgents did not return our agent).")

                Print("Successfully added agent to team")
        else:
            if:
                TeamCollection.AddToTeam[Agent, Teams[0]]
            then:
                # Recursively call our function again now that we are definitely on a team.
                OnAgentSpawned(Agent)
