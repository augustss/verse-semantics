using { /Fortnite.com/Devices }
using { /Fortnite.com/UI}
using { /Verse.org/Simulation }
using { /Fortnite.com/Characters }
using { /Verse.org/Colors }
using { /Verse.org/Simulation/Tags }
using { /Verse.org/Random }
using { /Fortnite.com/Game }
using { /Fortnite.com/Playspaces }
using { /UnrealEngine.com/Temporary/UI }

hud_controller_manager := class (creative_device):

    @editable
    MySpawners : []player_spawner_device = array{}

    OnEnd<override>():void=
        Print ("Nexus OnEnd - hud_controller_manager")

    OnBegin<override>()<suspends>:void=
        Print ("Nexus OnBegin - hud_controller_manager")
        for (Spawner : MySpawners):
            if (ASpawner := player_spawner_device[Spawner]):
                ASpawner.SpawnedEvent.Subscribe(HandleHUD)
            else:
                Print ("Nexus Error - hud_controller_manager.OnBegin() | Unable to get ASpawner from the array of player_spawner_device(s) OnBegin")

# Choosing the hud elements from the hud_element_identifier array to hide and hiding them for all the players in the Playspace
    HandleHUD(Agent: agent):void=
        Print("Hiding Hud")
        var HudElementsToHide: []hud_element_identifier = array
        {creative_hud_identifier_all{}, 
        hud_identifier_world_resource_wood{},
         hud_identifier_world_resource_stone{},
         hud_identifier_world_resource_metal{} }
        var HudInteractionElements: []hud_element_identifier = array{creative_hud_identifier_interaction_prompts{}}

        Playspace : fort_playspace = GetPlayspace()
        AllPlayers : []player = Playspace.GetPlayers()
            for (_Player : AllPlayers):
                if(FortChar : fort_character = _Player.GetFortCharacter[]):
                    if (PlayerUI := GetPlayerUI[_Player]):
                        PlayerUI.HideHUDElements(HudElementsToHide)
                        PlayerUI.HideHUDElements(HudInteractionElements)
                    else:
                        Print ("Nexus Error - hud_controller_manager.HandleHUD(Agent : agent) | Unable to GetPlayerUI from Player")
