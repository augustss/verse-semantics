
using { /Fortnite.com/Devices }
using { /Fortnite.com/Game }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }


claim_zone_manager := class(creative_device):
    var OnlyRandomPlayers : [player]newonlyrandom_player = map {}
    var Class : int = 0

    @editable
    ZoneClass : int = 0

    @editable
    ZoneClaim : mutator_zone_device = mutator_zone_device {}

    @editable
    ZonesText : []creative_prop = array {}

    @editable
    ZoneSelector : class_and_team_selector_device = class_and_team_selector_device {}


    OnBegin<override>()<suspends>:void=
        ZoneSelector.ClassSwitchedEvent.Subscribe(OnClassSwitched)
        ZoneClaim.AgentEntersEvent.Subscribe(OnPlayerEnter)
        GetPlayspace().PlayerRemovedEvent().Subscribe(OnPlayerLeftServer)

    OnClassSwitched(Agent : agent):void=
        ZoneClaim.Disable()
        for (ZoneText : ZonesText):
            ZoneText.Hide()

        if (_Player := player[Agent]):
            if (ExistingPlayer := OnlyRandomPlayers[_Player]):
                Print ("OnlyRandomPlayer exists already")
            else:
                Print ("New Player")
                OnlyRandomPlayer := newonlyrandom_player{Player := _Player}
                OnlyRandomPlayer.SetClass(ZoneClass)

                if (set OnlyRandomPlayers[_Player] = OnlyRandomPlayer) {}

    OnPlayerLeftServer(LeavingPlayer : player):void=
        if (ExistingPlayer := OnlyRandomPlayers[LeavingPlayer], Agent := agent[LeavingPlayer]):
            set Class = ExistingPlayer.GetClass()
            if (Class = ZoneClass):
                Print("A player has left successfully. Enabling a plot for claiming. Class idx is: {Class}")
                ZoneClaim.Enable()
                for (ZoneText : ZonesText):
                    ZoneText.Show()
            else:
            
            var NewOnlyRandomPlayerMap : [player]newonlyrandom_player = map {}
                
            for (Key -> Value : OnlyRandomPlayers, Key <> LeavingPlayer):
                set NewOnlyRandomPlayerMap = ConcatenateMaps( NewOnlyRandomPlayerMap, map{Key => Value})
            
            set OnlyRandomPlayers = NewOnlyRandomPlayerMap


    OnPlayerEnter(Agent : agent):void=
        ZoneSelector.ChangeClass(Agent)
