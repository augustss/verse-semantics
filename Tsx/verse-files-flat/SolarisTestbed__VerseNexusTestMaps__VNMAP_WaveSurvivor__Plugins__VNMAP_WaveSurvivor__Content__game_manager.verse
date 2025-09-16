
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }

game_manager := class(creative_device):

    @editable
    PlayerManager:player_manager := player_manager{}
    @editable
    EnemyManager:enemy_manager := enemy_manager{}
    @editable
    HUDManager:hud_device := hud_device{}
    @editable
    BonusManager:bonus_manager := bonus_manager{}
    
    PropsRandomMan:props_random_device := props_random_device{}

    OnBegin<override>()<suspends>:void=
        #Player functions
        PlayerManager.PlayerSpawner.SpawnedEvent.Subscribe(PlayerManager.OnPlayerSpawn)

        #Enemy functions
        EnemyManager.EnemySubscribers()

        #HUD functions
        spawn{HUDManager.HUDOnStartSetting()}

        #Utils functions
        PropsRandomMan.EnvironmentRandomising()
        BonusManager.SetupBonusArray()


        