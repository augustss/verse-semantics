using { /Fortnite.com/Devices }
using { /UnrealEngine.com/Temporary/UI }
using { /Verse.org/Simulation }

MinSpawnCooldown: float = 10.0
MaxSpawnerTier: int = 3


lane_team_device := class(creative_device):
    # list of guard spawners, edited in UEFN, each player is assigned one

    @editable
    Sentry:sentry_device = sentry_device{}

    @editable
    EnemyObjective:objective_device = objective_device{}

    @editable
    TeamEndGame:end_game_device = end_game_device{}

    @editable
    TeamPlayerSpawner:player_spawner_device = player_spawner_device{}

    @editable
    HudMessage:hud_message_device = hud_message_device{}

    @editable
    ShowUpgradeUIButton:button_device = button_device{}

    var PlayerVariables:[agent]player_variables = map{}

    var FirstTeamPlayer:?agent = false

    var Spawners:[int][]guard_spawner_device = map{}

    var PlayerIndex:int = 0

    OnBegin<override>()<suspends>:void =
        # set up the island
        Print("Nexus OnBegin - lane_team_device")
        Sentry.Disable()
        set Spawners = CollectSpawners(Self)
        ShowUpgradeUIButton.InteractedWithEvent.Subscribe(OnShowUpgradeUI)
        TeamPlayerSpawner.SpawnedEvent.Subscribe(OnSpawn)
        EnemyObjective.DestroyedEvent.Subscribe(OnDestroy)
        HudMessage.SetDisplayTime(2.0)

    OnEnd<override>():void =
        Print("Nexus OnEnd - lane_team_device")

    TimedSpawn(Agent:agent)<suspends>:void =
        # spawn a player's guards
        loop:
            if(Vars := PlayerVariables[Agent]):
                Vars.GuardSpawner.Spawn()
                HudMessage.Show(Agent, Message("Guard spawned!"))
                Sleep(Vars.SpawnCooldown)
            else:
                Print("NexusError - couldn't get player_variables for player!")

    OnShowUpgradeUI(Agent:agent):void =
        if (InPlayer := player[Agent], PlayerUI := GetPlayerUI[InPlayer], UIStruct := PlayerVariables[Agent].UIStruct?):
            PlayerUI.AddWidget(UIStruct.Canvas, player_ui_slot{InputMode := ui_input_mode.All})
        else:
            Print("NexusError - couldn't display upgrade UI for player!")

    OnDestroy(Agent:agent):void =
        # trigger round victory
        if (P:=FirstTeamPlayer?):
            TeamEndGame.Activate(P)

    OnSpawn(Agent:agent):void =
        # bind a 'variables' object to a player
        UIStruct := CreateUpgradeUI()
        if (not FirstTeamPlayer?):
            # set the player to trigger a win - currently AI agents cannot be passed to end_game_device.Activate()
            set FirstTeamPlayer = option{Agent}
        if:
            GuardSpawner := Spawners[1][PlayerIndex]
            set PlayerVariables[Agent] = player_variables:
                GuardSpawner:=GuardSpawner
                PlayerIndex:=PlayerIndex
            Variables := PlayerVariables[Agent]
            set PlayerIndex += 1
        then:
            UIStruct.UpgradeCooldownButton.OnClick().SubscribeWidgetMessage(OnPressUpgradeCooldown, Variables)
            UIStruct.UpgradeWeaponButton.OnClick().SubscribeWidgetMessage(OnPressUpgradeWeapon, (Variables, Spawners))
            set Variables.UIStruct = option{UIStruct}
            GuardSpawner.EliminatingEvent.SubscribeInteraction(OnElimination, Agent)
            spawn:
                TimedSpawn(Agent)
        else:
            Print("NexusError - Player variables assignment failed!")

    OnElimination(Interaction:device_ai_interaction_result, PlayerAgent:agent):void =
        if(Variables := PlayerVariables[PlayerAgent], UIStruct := Variables.UIStruct?):
            set Variables.Eliminations += 1
            UIStruct.EliminationsTextBlock.SetText(Message("Eliminations: {Variables.Eliminations}"))
            HudMessage.Show(PlayerAgent, Message("Elimination scored!"))