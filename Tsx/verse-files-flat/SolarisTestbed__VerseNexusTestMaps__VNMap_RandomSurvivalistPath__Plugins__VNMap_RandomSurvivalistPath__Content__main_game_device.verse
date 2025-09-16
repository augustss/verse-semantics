
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Random }
using { /Fortnite.com/Characters }
using { /UnrealEngine.com/Temporary/SpatialMath }
using { /Fortnite.com/UI }
using { /UnrealEngine.com/Temporary/UI }

main_game_device := class(creative_device):

    @editable
    PlayerSpawnerDevice<private> : player_spawner_device = player_spawner_device{}

    @editable
    var TeleporterDevicesArray<private> : []teleporter_device = array{}

    @editable
    ObjectsNearCharacter<private> : objects_near_character = objects_near_character{}

    @editable
    CampfireDevice<private> : campfire_device = campfire_device{}

    @editable
    MainEventDescriptionDevice<private> : main_event_description_device = main_event_description_device{}

    @editable
    PhysicsTreeDevice : physics_tree_device = physics_tree_device{}

    @editable
    ExplosiveDevice : explosive_device = explosive_device{}

    RandomEventCallerClass : random_event_caller = random_event_caller{}

    BaseLocationSetupEvent<private> : event() = event(){}
    var _Player : player = player{}
    TextNewEventButtonUI<localizes> : message = "Run new event"

    OnLogDestroyedEvent():void=
        Print("OnLogDestroyedEvent")   

    OnStumpDestroyedEvent():void=
        Print("OnStumpDestroyedEvent")    

    OnTreeKnockedDownEvent():void=
        Print("OnTreeKnockedDownEvent")    

    HandleNewEventButtonUI(Message : widget_message):void=
        RandomEventCallerClass.CallRandomEvent(MainEventDescriptionDevice.RunRandomizedEvent, ())

    CreateNewEventButtonUI() : canvas =

        EventButton : button_loud = button_loud{DefaultText := TextNewEventButtonUI}
 
        EventButton.OnClick().Subscribe(HandleNewEventButtonUI)
    
        MyInteractableButtons : canvas = canvas:
            Slots := array:
                canvas_slot:
                    Anchors := anchors{Minimum := vector2{X := 0.0, Y := 1.0}, Maximum := vector2{X := 0.0, Y := 1.0}}
                    Offsets := margin{Top := 0.0, Left := 0.0, Right := 0.0, Bottom := 0.0}
                    Alignment := vector2{X := 0.0, Y := 1.0}
                    SizeToContent := true
                    Widget := EventButton
    
        return MyInteractableButtons

    OnBegin<override>()<suspends>:void=
        Print("OnBegin - main_game_device")
        PhysicsTreeDevice.LogDestroyedEvent.Subscribe(OnLogDestroyedEvent)
        PhysicsTreeDevice.StumpDestroyedEvent.Subscribe(OnStumpDestroyedEvent)
        PhysicsTreeDevice.TreeKnockedDownEvent.Subscribe(OnTreeKnockedDownEvent)
        if (MaybePlayer := GetPlayspace().GetPlayers()[ GetPlayspace().GetPlayers().Length-1]):
            set _Player = MaybePlayer
            if (PlayerUI := GetPlayerUI[_Player]):
                PlayerUI.AddWidget(CreateNewEventButtonUI(), player_ui_slot{InputMode := ui_input_mode.All})

        set ObjectsNearCharacter.CampfireDevice = CampfireDevice

        # Calling first
        PlayerSpawnerDevice.SpawnedEvent.Subscribe(TeleportAgentInRandomLocation)
        Print("BaseLocationSetupEvent.Await()")            
        BaseLocationSetupEvent.Await()
        Print("NexusInfo - campfire set up in main_game_device")            
        CampfireDevice.Light(_Player)

        ObjectsNearCharacter.TeleportObjectNearToPlayer(PhysicsTreeDevice, _Player)
        ExplosiveDevice.Explode(_Player)
        PhysicsTreeDevice.ReleaseLog()
        PhysicsTreeDevice.DestroyLog()
        PhysicsTreeDevice.DestroyStump()

        # Choose random event to call
        RandomEventCallerClass.CallRandomEvent(MainEventDescriptionDevice.RunRandomizedEvent, ())
            
    DisableAllTeleports()<suspends>:void=
        for (Device : TeleporterDevicesArray):
            # Calling second in parallel
            Device.Disable()  

    TeleportAgentInRandomLocation<private>(Agent : agent):void=
        Print("TeleportAgentInRandomLocation")            
        set TeleporterDevicesArray = Shuffle(TeleporterDevicesArray)
        Print("Shuffle(TeleporterDevicesArray)")            

        if (TeleporterDevice := TeleporterDevicesArray[0]):
            Print("TeleporterDevice.TeleportedEvent.SubscribeWithEvent(ObjectsNearCharacter.PlaceNecessaryObjects, BaseLocationSetupEvent)")            
            TeleporterDevice.TeleportedEvent.SubscribeWithEvent(ObjectsNearCharacter.PlaceNecessaryObjects, BaseLocationSetupEvent)
            TeleporterDevice.Teleport(Agent)
            spawn{DisableAllTeleports()}  

