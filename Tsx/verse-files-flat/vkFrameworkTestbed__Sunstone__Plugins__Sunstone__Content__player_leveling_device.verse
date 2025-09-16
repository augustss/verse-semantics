
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

player_level_data:= class<concrete>:
    @editable
    XPRequirement :int = 0
    @editable
    GoldMultiplier :float = 1.0
    @editable
    XPMultiplier :float = 1.0

player_leveling_device := class(creative_device):
    @editable
    InfantrySelectorPerLevel : []class_and_team_selector_device = array{}
    @editable
    HeavySelectorPerLevel : []class_and_team_selector_device = array{}
    @editable
    ScoutSelectorPerLevel: []class_and_team_selector_device = array{}

    @editable
    PlayerLevels : []player_level_data = array{}

    @editable
    ClassSelectorUIPopup : popup_dialog_device = popup_dialog_device{}
    
    @editable
    ClassSelectorUIPopup2 : popup_dialog_device = popup_dialog_device{}

    @editable
    PlayerProfileManger : player_profile_manager = player_profile_manager{}

    @editable
    CharacterScreenUI : hud_message_device  = hud_message_device{}

    var EventsManagerRef:?global_events_manager = false

    @editable
    DebugButton : button_device = button_device{}

    OnBegin<override>()<suspends>:void=
        DebugButton.InteractedWithEvent.Subscribe(DebugButtonPress)
        Self.GetPlayspace().PlayerAddedEvent().Subscribe(InitialisePlayerUI)

        # Class Selector Dialog is duplicated & communicates on 2 different channels
        ClassSelectorUIPopup.RespondingButtonEvent.Subscribe(HandleClassSwitch)
        ClassSelectorUIPopup2.RespondingButtonEvent.Subscribe(HandleClassSwitch)

        if (EventsManager := global_events_accessor{}.Instance[Self]):
            set EventsManagerRef = option{EventsManager}
            if:
                EventsManager.RegisterEvent[event_type.Player, HandleXPEarned]
            else:
                Print("Failed to Subscribe to XPEarned event")

        InitialisePlayersUI()    

    InitialisePlayersUI():void=
        for (Player : Self.GetPlayspace().GetPlayers()):
            InitialisePlayerUI(Player)


    InitialisePlayerUI(Player : player):void=
        if:
            LevelData :=  PlayerLevels[1] 
            RequireXP := LevelData.XPRequirement
        then:
            UpdateOnScreenUI(Player, 1, RequireXP, false)

    HandleXPEarned(SubEvent: sub_events, Agent : ?agent, EarnedXP : ?int):void=
        # grab the players data 

        if:
            SubEvent = sub_events.XPEarned
            XP := EarnedXP?
            SessionData := PlayerProfileManger.GetSessionPlayerData[Agent?]
            CurrentXP := SessionData.CurrentXP
            DataIndex := SessionData.PlayerLevel
            CurrentClass := SessionData.CurrentClass
            CurrentLevelData :=  PlayerLevels[DataIndex -1]
            NextLevelData := PlayerLevels[DataIndex]
            ModifiedXP := Floor[XP * CurrentLevelData.XPMultiplier]
            AgentRef := Agent?
            player[AgentRef]
        then:
            if ((ModifiedXP + CurrentXP) >= NextLevelData.XPRequirement):
                #Leveled up

                Print("You leveled up new level {SessionData.PlayerLevel + 1}")

                NewData := session_player_data:
                    MakeSessionData<constructor>(SessionData)
                    PlayerLevel := SessionData.PlayerLevel + 1
                    CurrentXP := SessionData.CurrentXP + ModifiedXP
                    GoldMultiplier := NextLevelData.GoldMultiplier
                    XPMultiplier := NextLevelData.XPMultiplier

                if:
                    PlayerProfileManger.UpdateSessionPlayerData[AgentRef, NewData]
                else:
                    Print("Failed To update session data on level up")

                LevelUpClass(CurrentClass,AgentRef, DataIndex)

                # ping event for UI system to listen to
                if:
                    GEM := EventsManagerRef?
                then:
                    GEM.TriggerEvent(event_type.Player, sub_events.LevelUp, ?Agent := Agent)

                    if:
                        LevelData :=  PlayerLevels[NewData.PlayerLevel] 
                        RequireXP := Max(0, LevelData.XPRequirement - ModifiedXP - CurrentXP)
                    then:
                        UpdateOnScreenUI(AgentRef, NewData.PlayerLevel, RequireXP, logic {PlayerLevels.Length <= NewData.PlayerLevel})
                    else:
                        UpdateOnScreenUI(AgentRef, NewData.PlayerLevel, 0, true)
            else:
                Print("XP Gained {ModifiedXP}")
                #Just update profile with new XP
                NewData := session_player_data:
                    MakeSessionData<constructor>(SessionData)
                    CurrentXP := SessionData.CurrentXP + ModifiedXP
                if:
                    PlayerProfileManger.UpdateSessionPlayerData[AgentRef, NewData]
                else:
                    Print("Failed To update session data on XP Gain")

                RequireXP := Max(0, NextLevelData.XPRequirement - ModifiedXP - CurrentXP)
                UpdateOnScreenUI(AgentRef, DataIndex, RequireXP, logic {PlayerLevels.Length = DataIndex})

    # Button 1 - Infantry, Button 2 - Scout, Button 3 - Heavy
    HandleClassSwitch(Input: tuple(agent, int)):void=
        if:
            Agent := Input(0)
            ButtonIndex := Input(1)
            SessionData := PlayerProfileManger.GetSessionPlayerData[Agent]
            CurrentClass := SessionData.CurrentClass
            CurrentLevelIndex := SessionData.PlayerLevel - 1
        then:
            # This has potential for content error if classes on the popup device don't match the expected button indexes
            var ClassSelected : player_classes = CurrentClass
            if (ButtonIndex = 0):
                set ClassSelected = player_classes.Infantry
            else if (ButtonIndex = 1):
                set ClassSelected = player_classes.Scout
            else if (ButtonIndex = 2):
                set ClassSelected = player_classes.Heavy

            if (CurrentClass <> ClassSelected):
                NewData := session_player_data:
                    MakeSessionData<constructor>(SessionData)
                    CurrentClass := ClassSelected
                if:
                    PlayerProfileManger.UpdateSessionPlayerData[Agent, NewData]
                else:
                    Print("Failed To update session data on class switch")

                LevelUpClass(ClassSelected, Agent, CurrentLevelIndex)
            else:
                LevelUpClass(CurrentClass, Agent, CurrentLevelIndex)

    DebugButtonPress(Agent:agent): void=
        if:
            Ref := EventsManagerRef?
        then:
            Ref.TriggerEvent(event_type.Player, sub_events.XPEarned, ?Agent := option{Agent}, ?IntArg:= option{10})

    LevelUpClass(PlayerClass : player_classes, Agent : agent, Index:int):void=
        case (PlayerClass):
            player_classes.Infantry =>
                if :
                    Selector := InfantrySelectorPerLevel[Index]
                then:
                    Selector.ChangeClass(Agent)
            player_classes.Heavy =>
                if :
                    Selector := HeavySelectorPerLevel[Index]
                then:
                    Selector.ChangeClass(Agent)
            player_classes.Scout =>
                if :
                    Selector := ScoutSelectorPerLevel[Index]
                then:
                    Selector.ChangeClass(Agent)


    PrintMessage<localizes>(Level:int, NxtLevel:int, XPRequired: int):message = "LEVEL: {Level}                  {XPRequired}XP to LV{NxtLevel}"

    PrintMessageMax<localizes>(Level:int):message = "LEVEL: {Level}              Max Level Reached"

    UpdateOnScreenUI(Agent: agent, CurrentLevel: int, XPRequired : int, IsMaxLevel : logic):void=
        if (IsMaxLevel?):
            CharacterScreenUI.Show(Agent, PrintMessageMax(CurrentLevel))
        else:
            CharacterScreenUI.Show(Agent, PrintMessage(CurrentLevel, CurrentLevel +1,  XPRequired))